/*  C MODULE: org-agenda-lock.c
 *
 *  Purpose   : Emacs Org Agenda Lock Manager.
 *  Created   : Wednesday, October 21 2026.
 *  Author    : Pierre Rouleau <prouleau001@gmail.com>
 *  Time-stamp: <2026-10-21 22:54:02 EDT, updated by Pierre Rouleau>
 *  Copyright © 2026, Pierre Rouleau
 *  License   : MIT
 */
/* -------------------------------------------------------------------------- */
/* Module Description
 * ------------------
 */



/* -------------------------------------------------------------------------- */
/* Header Inclusion
 * ----------------
 */

#define _DEFAULT_SOURCE
#define _POSIX_C_SOURCE 200809L

#ifdef __APPLE__
  /* The following is required to get `flock' prototype from <sys/file.h> */
  #define _DARWIN_C_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>

/* -------------------------------------------------------------------------- */
/* Local Types
 * -----------
 */



/* -------------------------------------------------------------------------- */
/* Local Variables
 * ---------------
 */



/* -------------------------------------------------------------------------- */
/* Code
 * ----
 */

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <lock_path> <emacs_pid>\n", argv[0]);
        return 1;
    }

    const char *lock_path = argv[1];
    long emacs_pid = strtol(argv[2], NULL, 10);

    // Open file securely with O_NOFOLLOW
    int flags = O_CREAT | O_RDWR | O_NOFOLLOW;
    int fd = open(lock_path, flags, 0600);
    if (fd < 0) {
        perror("Error opening lock file");
        return 1;
    }

    // Validate file integrity
    struct stat info;
    if (fstat(fd, &info) < 0) {
        perror("Error stating lock file");
        close(fd);
        return 1;
    }

    if (!S_ISREG(info.st_mode)) {
        fprintf(stderr, "RuntimeError: Agenda lock is not a regular file\n");
        close(fd);
        return 1;
    }

    if (info.st_uid != getuid() || info.st_nlink != 1) {
        fprintf(stderr, "RuntimeError: Agenda lock has an unexpected owner or link count\n");
        close(fd);
        return 1;
    }

    // Force strict 0600 permissions
    if (fchmod(fd, 0600) < 0) {
        perror("Error setting file permissions");
        close(fd);
        return 1;
    }

    // Attempt non-blocking exclusive lock
    if (flock(fd, LOCK_EX | LOCK_NB) < 0) {
        if (errno == EWOULDBLOCK || errno == EAGAIN) {
            printf("BUSY\n");
            fflush(stdout);
            close(fd);
            return 0;
        } else {
            perror("Error locking file");
            close(fd);
            return 1;
        }
    }

    // Wipe file content safely only after successfully acquiring lock
    if (ftruncate(fd, 0) < 0 || lseek(fd, 0, SEEK_SET) < 0) {
        perror("Error truncating/seeking lock file");
        close(fd);
        return 1;
    }

    // Gather runtime system data
    char hostname[256];
    if (gethostname(hostname, sizeof(hostname)) < 0) {
        strncpy(hostname, "unknown", sizeof(hostname));
    }
    hostname[sizeof(hostname) - 1] = '\0'; // Guarantee null-termination

    long helper_pid = (long)getpid();

    // Map system file descriptor into standard buffered stream
    FILE *record = fdopen(fd, "r+");
    if (!record) {
        perror("Error creating file stream");
        close(fd);
        return 1;
    }

    // Output JSON details
    fprintf(record, "{\"version\": 1, \"host\": \"%s\", \"emacs_pid\": %ld, \"helper_pid\": %ld}\n",
            hostname, emacs_pid, helper_pid);
    fflush(record);

    // Notify parent harness
    printf("READY\n");
    fflush(stdout);

    // Read standard input until an EOF condition is met
    char buffer[4096];
    while (fread(buffer, 1, sizeof(buffer), stdin) > 0) {
        // Discard buffer payload, purely staying alive
    }

    // Clean execution tear down
    fclose(record);
    return 0;
}

/* -------------------------------------------------------------------------- */
