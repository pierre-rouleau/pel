/*  C MODULE: lses.c
 *
 *  Purpose   : Print information about currently running Emacs daemons.
 *  Created   : Friday, October  9 2026.
 *  Author    : Pierre Rouleau <prouleau001@gmail.com>
 *  Time-stamp: <2026-10-20 16:10:24 EDT, updated by Pierre Rouleau>
 *  Copyright © 2026, Pierre Rouleau
 *  License   : GNU GPL v3
 */
/* -------------------------------------------------------------------------- */
/* Module Description
 * ------------------
 *
 * lses: a C99 single file program that prints information about Emacs daemon
 *       servers and sockets.
 *
 * Program features:
 * -----------------
 *
 * - Supports Linux and macOS. It does not support other platforms.
 * - Supports Emacs 26.1 and later:
 *   - Does not use emacsclient --timeout, which Emacs 26.1 lacks.
 *
 * - Confirms responsive daemons and reports their PIDs.
 * - Identifies responsive non-daemon Emacs servers.
 * - Reports a stale socket only after ECONNREFUSED, with an unchanged socket entry.
 * - Does not verify timeouts, permission errors, and other failures. unverified.
 * - Does not start Emacs.
 * - Does not delete any socket.
 *
 *
 * Exit status:
 * ------------
 *
 *   0: at least one responsive Emacs daemon confirmed.
 *   1: local program error, or emacsclient could not be executed.
 *   2: no responsive Emacs daemon confirmed.
 *
 *
 * Implementation Notes:
 * ---------------------
 *
 * - Requires POSIX.1-2008 : should be OK for most existing Linux distros and
 *   macOS systems.
 *
 * - Can be built with GNU make in the current directory with:
 *    - make lses
 *    - make lses CFLAGS="-std=c99 -O2 -Wall -Wextra -Wpedantic"
 *
 */

/* -------------------------------------------------------------------------- */
/* Header Inclusion
 * ----------------
 */

/* Control what is provided by system headers */
#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE   700
#ifdef __APPLE__
#define _DARWIN_C_SOURCE       /* exposes _CS_DARWIN_USER_TEMP_DIR */
                               /* to scan Darwin per-user temporary directory when TMPDIR is unset */
#endif

/* Include system headers */
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>             /* exit(),  */
#include <string.h>             /* strdup(in POSIX) */
#include <stdbool.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>

/* -------------------------------------------------------------------------- */
/* Local Types
 * -----------
 */

#define CONNECT_MS 1000
#define CLIENT_MS  3000

struct paths {
    char **items;
    size_t count;
};

/* -------------------------------------------------------------------------- */
/* Local Variables
 * ---------------
 */

static struct paths sockets;
static struct paths directories;
static int execution_error;

/* -------------------------------------------------------------------------- */
/* Macros */

#define STR_HELPER(x) #x
#define STR(x)        STR_HELPER(x)

#define FORMAT_MSG(msg) "Error @ " STR(__LINE__) ": " msg

/* The DIE macro prepend the line number to the function name passed as msg */
#define DIE(msg) die(FORMAT_MSG(msg))

/* -------------------------------------------------------------------------- */
/* Code
 * ----
 */

static void
die(const char *message)
{
    perror(message);
    exit(1);
}

static char *
copy_string(const char *s)
{
    char *p = strdup(s);
    if (!p) DIE("strdup");
    return p;
}


static char *
join_path(const char *directory, const char *name)
{
    size_t d = strlen(directory);
    size_t n = strlen(name);
    int with_separator = d > 0 && directory[d - 1] != '/';
    char *p = malloc(d + (with_separator?1:0) + n + 1);

    if (!p) DIE("malloc");

    memcpy(p, directory, d);
    if (with_separator)
        p[d++] = '/';
    memcpy(p + d, name, n + 1);
    return p;
}


/* Record path inside the identified paths list
 *   - In this program: should be the global `socket' or `directories' variables.
 * Return true for a new path, or false for an existing path.
 */
static bool
add_path(struct paths *list, const char *path)
{
    /* Is path already recorded inside the specified list? */
    size_t i;
    for (i = 0; i < list->count; ++i)
        if (strcmp(list->items[i], path) == 0)
            return false;       /* found! */

    /* It's not already there, add it: need one extra char* */
    char **items = realloc(list->items, (list->count + 1) * sizeof(*items));
    if (!items) DIE("realloc");

    list->items = items;
    list->items[list->count++] = copy_string(path);
    return true;                /* new one recorded */
}

static void
free_paths(struct paths *list)
{
    size_t i;

    for (i = 0; i < list->count; ++i)
        free(list->items[i]);
    free(list->items);
}

static long long
now_ms(void)
{
    struct timespec ts;

    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) DIE("clock_gettime");

    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* EINTR does not extend the deadline. */
static int
poll_until(struct pollfd *pfd, long long deadline)
{
    for (;;) {
        long long remaining = deadline - now_ms();
        int result;

        if (remaining <= 0) return 0;

        result = poll(pfd, 1, (int)remaining);
        if (result >= 0)     return result;
        if (errno != EINTR)  return -1;
    }
}

static void
report(const char *path, const char *status, const char *detail)
{
    char name_block[256];
    const char *name = strrchr(path, '/');

    snprintf(name_block, sizeof(name_block), "[Name: %s]", name ? name + 1 : path);

    printf("-> %s: %-16s [Path: %s]",
           status, name_block, path);

    if (detail && *detail)
        printf(" [%s]", detail);

    putchar('\n');
}

static int
same_socket(const struct stat *a, const struct stat *b)
{
    return S_ISSOCK(b->st_mode)
           && a->st_dev == b->st_dev
           && a->st_ino == b->st_ino;
}

/*
 * Recheck the pathname before reporting a result.
 * This reduces replacement races; it does not make the check atomic.
 */
static bool
socket_unchanged(const char *path, const struct stat *original)
{
    struct stat current;

    if (lstat(path, &current) != 0) {
        int error = errno;
        report(path,
               error == ENOENT ? "Socket disappeared" : "Socket unverified",
               error == ENOENT ? NULL : strerror(error));
        return false;
    }

    if (!same_socket(original, &current)) {
        report(path, "Socket unverified", "socket changed during check");
        return false;
    }

    return true;
}

/* Return 0 for a listener, or an errno value for failure. */
static int
check_listener(const char *path)
{
    struct sockaddr_un address;
    struct pollfd pfd;
    socklen_t length;
    int fd, flags, error, result;
    long long deadline = now_ms() + CONNECT_MS;
    size_t n = strlen(path);

    if (n >= sizeof(address.sun_path))
        return ENAMETOOLONG;

    memset(&address, 0, sizeof(address));
    address.sun_family = AF_UNIX;
    memcpy(address.sun_path, path, n + 1);

    length = (socklen_t)(offsetof(struct sockaddr_un, sun_path) + n + 1);

#ifdef __APPLE__
    address.sun_len = (unsigned char)length;
#endif

    fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (fd < 0) return errno;

    flags = fcntl(fd, F_GETFL);
    if (flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
        error = errno;
        close(fd);
        return error;
    }

    if (connect(fd, (struct sockaddr *)&address, length) == 0) {
        close(fd);
        return 0;
    }

    error = errno;
    if (error != EINPROGRESS) {
        /*
         * In particular, Linux AF_UNIX EAGAIN is not treated as a
         * pending connection. It remains an unverified failure.
         */
        close(fd);
        return error;
    }

    pfd.fd = fd;
    pfd.events = POLLOUT;
    pfd.revents = 0;

    result = poll_until(&pfd, deadline);
    if (result == 0) {
        error = ETIMEDOUT;
    } else if (result < 0) {
        error = errno;
    } else {
        socklen_t size = sizeof(error);

        if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &error, &size) != 0)
            error = errno;
        else if (error == 0 && !(pfd.revents & POLLOUT))
            error = EIO;
    }

    close(fd);
    return error;
}

static void
kill_and_reap(pid_t child)
{
    int status;

    /*
     * The child has not been reaped, so its PID cannot be reused here.
     * SIGKILL bounds cleanup without a second grace-period timeout.
     */
    if (kill(child, SIGKILL) < 0 && errno != ESRCH)
        perror("kill");

    while (waitpid(child, &status, 0) < 0) {
        if (errno != EINTR)
            break;
    }
}

/*
 * Return:
 *    0: successful client response
 *   -1: client or pipe failure
 *   -2: response deadline expired
 *   -3: client execution/setup failure
 *
 * Capture bounded stdout. Discard stderr. Do not invoke a shell.
 */
static int
query_emacs(const char *path, char *output, size_t capacity)
{
    static const char expression[] =
        "(format \"%s:%d\" "
        "(if (daemonp) \"daemon\" \"server\") (emacs-pid))";

    int pipefd[2], flags, status = 0;
    int eof = 0, reaped = 0;
    size_t used = 0;
    pid_t child;
    long long deadline = now_ms() + CLIENT_MS;

    if (pipe(pipefd) != 0)
        return -1;

    flags = fcntl(pipefd[0], F_GETFL);
    if (flags < 0 ||
        fcntl(pipefd[0], F_SETFL, flags | O_NONBLOCK) < 0) {
        close(pipefd[0]);
        close(pipefd[1]);
        return -1;
    }

    child = fork();
    if (child < 0) {
        close(pipefd[0]);
        close(pipefd[1]);
        return -1;
    }

    if (child == 0) {
        int nullfd;

        close(pipefd[0]);
        nullfd = open("/dev/null", O_RDWR);
        if (nullfd < 0)  _exit(126);

        if (dup2(nullfd,    STDIN_FILENO)  < 0 ||
            dup2(pipefd[1], STDOUT_FILENO) < 0 ||
            dup2(nullfd,    STDERR_FILENO) < 0)
            _exit(126);

        if (nullfd > STDERR_FILENO)     close(nullfd);
        if (pipefd[1] > STDERR_FILENO)  close(pipefd[1]);

        /* With no alternate editor, connection failure exits instead
         * of launching another program or starting an Emacs daemon.
         *
         * Remove TCP-server and TRAMP overrides so the query addresses
         * only the selected local Unix socket.
         */
        if (unsetenv("ALTERNATE_EDITOR") != 0 ||
            unsetenv("EMACS_SERVER_FILE") != 0 ||
            unsetenv("EMACSCLIENT_TRAMP") != 0)
            _exit(126);

        execlp("emacsclient", "emacsclient",
               "--socket-name", path,
               "--eval", expression, (char *)NULL);
        _exit(127);
    }

    close(pipefd[1]);

    for (;;) {
        struct pollfd pfd;
        int result;
        long long remaining;

        if (!eof) {
            ssize_t n;

            if (used == capacity - 1)
                goto failed;

            n = read(pipefd[0], output + used, capacity - 1 - used);
            if (n > 0) {
                used += (size_t)n;
            } else if (n == 0) {
                eof = 1;
            } else if (errno != EAGAIN && errno != EWOULDBLOCK &&
                       errno != EINTR) {
                goto failed;
            }
        }

        if (!reaped) {
            pid_t result_pid = waitpid(child, &status, WNOHANG);

            if (result_pid == child)
                reaped = 1;
            else if (result_pid < 0 && errno != EINTR)
                goto failed;
        }

        if (eof && reaped)
            break;

        remaining = deadline - now_ms();
        if (remaining <= 0) {
            close(pipefd[0]);
            if (!reaped)
                kill_and_reap(child);
            return -2;
        }

        /*
         * Periodically check child exit, including after pipe EOF.
         * Never block in waitpid while the child is still running.
         */
        pfd.fd = eof ? -1 : pipefd[0];
        pfd.events = POLLIN;
        pfd.revents = 0;

        result = poll(&pfd, 1, remaining > 20 ? 20 : (int)remaining);
        if (result < 0 && errno != EINTR)
            goto failed;
    }

    close(pipefd[0]);
    output[used] = '\0';

    if (!WIFEXITED(status)) return -1;
    if (WEXITSTATUS(status) == 126 || WEXITSTATUS(status) == 127) return -3;
    return WEXITSTATUS(status) == 0 ? 0 : -1;

failed:
    close(pipefd[0]);
    if (!reaped)
        kill_and_reap(child);
    return -1;
}

/* Accept only a quoted daemon/server marker with a positive PID. */
static int
parse_response(char *reply, int *daemon, long *pid)
{
    char *p = reply;
    char *end;

    while (isspace((unsigned char)*p))
        ++p;

    if (strncmp(p, "\"daemon:", 8) == 0) {
        *daemon = 1;
        p += 8;
    } else if (strncmp(p, "\"server:", 8) == 0) {
        *daemon = 0;
        p += 8;
    } else {
        return 0;
    }

    if (!isdigit((unsigned char)*p))
        return 0;

    errno = 0;
    *pid = strtol(p, &end, 10);
    if (errno == ERANGE || *pid <= 0 || *end != '"')
        return 0;

    p = end + 1;
    while (isspace((unsigned char)*p))
        ++p;

    return *p == '\0';
}

static int
probe_socket(const char *path)
{
    struct stat original;
    char reply[256], detail[80];
    int error, result, daemon;
    long pid;

    if (lstat(path, &original) != 0) {
        error = errno;
        report(path,
               error == ENOENT ? "Socket disappeared" : "Socket unverified",
               error == ENOENT ? NULL : strerror(error));
        return 0;
    }

    if (!S_ISSOCK(original.st_mode)) {
        report(path, "Socket unverified", "path is no longer a socket");
        return 0;
    }

    error = check_listener(path);
    if (!socket_unchanged(path, &original))
        return 0;

    if (error != 0) {
        report(path,
               error == ECONNREFUSED ? "Socket stale" : "Socket unverified",
               error == ECONNREFUSED
                   ? "no listener at time of check"
                   : strerror(error));
        return 0;
    }

    result = query_emacs(path, reply, sizeof(reply));

    if (result == -3) execution_error = 1;

    if (!socket_unchanged(path, &original)) return 0;

    if (result != 0) {
        report(path, "Socket unverified",
               result == -2 ? "Emacs response timed out" :
               result == -3 ? "cannot execute emacsclient" :
                              "emacsclient or response capture failed");
        return 0;
    }

    if (!parse_response(reply, &daemon, &pid)) {
        report(path, "Socket unverified", "unexpected Emacs response");
        return 0;
    }

    snprintf(detail, sizeof(detail), "PID: %ld", pid);
    report(path,
           daemon ? "Daemon Running" : "Server Running (not a daemon)",
           detail);
    return daemon;
}

/*
 * Canonicalize the parent directory, not the socket itself.
 * Socket symlinks are excluded, as with the existing find -type s.
 */
static void
add_explicit_socket(const char *existing_emacsclient_socket_path)
{
    if (!existing_emacsclient_socket_path || existing_emacsclient_socket_path[0] != '/')
        return;

    struct stat info;
    char *parent, *slash, *resolved, *absolute;

    if (lstat(existing_emacsclient_socket_path, &info) != 0 || !S_ISSOCK(info.st_mode))
        return;

    parent = copy_string(existing_emacsclient_socket_path);
    slash = strrchr(parent, '/');
    *slash = '\0';

    resolved = realpath(*parent ? parent : "/", NULL);
    if (resolved) {
        absolute = join_path(resolved, slash + 1);
        add_path(&sockets, absolute);
        free(absolute);
        free(resolved);
    } else {
        fprintf(stderr, "lses: cannot resolve %s: %s\n",
                existing_emacsclient_socket_path, strerror(errno));
    }

    free(parent);
}

static void
scan_directory(const char *path)
{
    if (!path || !*path) return;

    char *resolved = realpath(path, NULL);

    if (!resolved) {
        if (errno != ENOENT && errno != ENOTDIR)
            fprintf(stderr, "lses: cannot resolve %s: %s\n",
                    path, strerror(errno));
        return;
    }

    if (!add_path(&directories, resolved)) {
        free(resolved);
        return;
    }

    DIR *directory = opendir(resolved);
    if (!directory) {
        if (errno != ENOTDIR)
            fprintf(stderr, "lses: cannot scan %s: %s\n",
                    resolved, strerror(errno));
        free(resolved);
        return;
    }

    for (;;) {
        struct dirent *entry;
        struct stat info;
        char *candidate;

        errno = 0;
        entry = readdir(directory);
        if (!entry) {
            if (errno != 0)
                fprintf(stderr, "lses: cannot read %s: %s\n",
                        resolved, strerror(errno));
            break;
        }

        if (strcmp(entry->d_name, ".") == 0 ||
            strcmp(entry->d_name, "..") == 0)
            continue;

        candidate = join_path(resolved, entry->d_name);
        if (lstat(candidate, &info) == 0) {
            if (S_ISSOCK(info.st_mode))
                add_path(&sockets, candidate);
        } else if (errno != ENOENT) {
            fprintf(stderr, "lses: cannot inspect %s: %s\n",
                    candidate, strerror(errno));
        }

        free(candidate);
    }

    closedir(directory);
    free(resolved);
}

static int
compare_paths(const void *a, const void *b)
{
    return strcmp(*(const char *const *)a, *(const char *const *)b);
}

static void
usage(const char* pgm_name)
{
    printf(
        "%s -- List current Emacs daemon servers & sockets.\n\n"
        "Usage: %s -h | --help\n\n"
        " • Print this help information.\n\n"
        "Usage: %s [-v | --verbose]\n\n"
        " • Report responsive Emacs daemons with their names, sockets and PIDs.\n"
        "   Also report responsive non-daemon servers and socket problems.\n"
        "   With --verbose, also print a summary when no responsive daemon\n"
        "   is confirmed.\n"
        "\n"
        "Requires emacsclient from Emacs 26.1 or later in PATH.\n"
        "Searches EMACS_SOCKET_NAME, XDG_RUNTIME_DIR/emacs,\n"
        "TMPDIR/emacsUID (default /tmp).\n"
        "On macOS it also searches the macOS temporary fallback.\n"
        "Searches immediate directory entries only.\n"
        "\n"
        "Connection deadline: 1 second per socket.\n"
        "Emacs response deadline: 3 seconds per socket.\n"
        "Never starts Emacs or deletes discovered sockets.\n"
        "\n"
        "Exit status:\n"
        "  0: at least one responsive Emacs daemon confirmed.\n"
        "  1: local program error or client execution failure.\n"
        "  2: no responsive Emacs daemon confirmed.\n\n"
        "LIMITATION: %s only supports Linux and macOS.\n", pgm_name, pgm_name, pgm_name, pgm_name
    );
}

int
main(int argc, char **argv)
{
    const char *pgm_name = strrchr(argv[0], '/');
    pgm_name = pgm_name ? pgm_name + 1 : argv[0];


    /* The EMACS_SOCKET_NAME environment variable specifies the filename or path
     * of the Unix domain socket that emacsclient uses to communicate with a
     * running Emacs server or daemon.
     */
    const char *selected = getenv("EMACS_SOCKET_NAME");
    const char *xdg = getenv("XDG_RUNTIME_DIR");
    const char *tmp = getenv("TMPDIR");
    char suffix[64];
    char *path;
    size_t i;
    int found = 0;
    bool verbose = false;

    /* Process command line options */
    if (argc == 2) {
        if ((strcmp(argv[1], "-h")        == 0) ||
            (strcmp(argv[1], "--help")   == 0)) {
            usage(pgm_name);
            return 0;
        }
        else if ((strcmp(argv[1], "-v")        == 0)  ||
                 (strcmp(argv[1], "--verbose") == 0)) {
            verbose = true;
        }
        else {
            fprintf(stderr, "lses: unexpected argument %s; use --help\n", argv[1]);
            return 1;
        }
    }
    if (argc > 2) {
        fprintf(stderr, "lses: unexpected argument; use --help\n");
        return 1;
    }

    /* Read User ID, used in the temporary folder name */
    snprintf(suffix, sizeof(suffix), "emacs%lu",
             (unsigned long)getuid());


    /* An inherited SIG_IGN would auto-reap children and break waitpid(). */
    /* exec keeps an ignored SIGCHLD disposition. If a parent process starts
     * lses with SIGCHLD set to SIG_IGN, the kernel reaps emacsclient
     * automatically. waitpid(child, &status, WNOHANG) then fails with ECHILD,
     * and the code takes goto failed. Every responsive socket then shows as
     * "emacsclient or response capture failed", and the exit status becomes
     * 2. Some service managers and scripts start programs this way. Restore the
     * default disposition once.
     */
    signal(SIGCHLD, SIG_DFL);

    add_explicit_socket(selected);
    scan_directory(selected);

    if (xdg && *xdg) {
        path = join_path(xdg, "emacs");
        scan_directory(path);
        free(path);
    }

    path = join_path(tmp && *tmp ? tmp : "/tmp", suffix);
    scan_directory(path);
    free(path);

#if defined(__APPLE__) && defined(_CS_DARWIN_USER_TEMP_DIR)
    {
        size_t size = confstr(_CS_DARWIN_USER_TEMP_DIR, NULL, 0);

        if (size > 0) {
            char *base = malloc(size);
            size_t actual;

            if (!base) DIE("malloc");

            actual = confstr(_CS_DARWIN_USER_TEMP_DIR, base, size);
            if (actual > 0 && actual <= size && *base) {
                path = join_path(base, suffix);
                scan_directory(path);
                free(path);
            }

            free(base);
        }
    }
#endif

    if (sockets.count > 1)
        qsort(sockets.items, sockets.count,
              sizeof(*sockets.items), compare_paths);

    for (i = 0; i < sockets.count; ++i) {
        if (probe_socket(sockets.items[i]))
            found = 1;
    }

    free_paths(&sockets);
    free_paths(&directories);

    if (verbose && !found) puts("No responsive Emacs daemons found.");

    if (execution_error) return 1;

    return found ? 0 : 2;
}

/* -------------------------------------------------------------------------- */
