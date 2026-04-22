// Class Exercice: Test PEL's ability to detect C++ code.
//
// Description:
//
//
//
// Diagnostic:
//
//
//
// Example:
//
//
//
// See Also:
//
//
//

class Exercice
{
   // - Exception Class:
   public:


   // - Types:
   public:


   // - Types:
   protected:


   // - Types:
   private:


   // - Static Class Constant Data Members:
   public:


   // - Static Class Function Members:
   public:


   // - Class Construction/Destruction/Operators:
   public:
   // Default constructor. Allows:
   //                      - Exercice value;
   //                      - Exercice arr[10];
   //                      - Exercice* pt = new Exercice[20];
   Exercice();

   // Exercice destructor: cleanup
   virtual ~Exercice();


   // - Function Members:
   public:


   // - Function Members:
   protected:


   // - Function Members:
   private:


   // - Static Class Data Members:
   public:


   // - Static Class Data Members:
   protected:


   // - Static Class Data Members:
   private:


   // - Private Forbidden Operators:
   private:
   // For pre-C++11:
   //lint -save
   //lint -esym(1526, Exercice::Exercice )
   //lint -esym(1704, Exercice::Exercice )
   //lint -esym(1714, Exercice::Exercice )
   // copy constructor.
   //   supports : Exercice a_Exercice = another_Exercice;
   //   and      : some_function(Exercice value)
   Exercice(Exercice const& other);

   //lint -esym(1526, Exercice::operator= )
   //lint -esym(1704, Exercice::operator= )
   //lint -esym(1714, Exercice::operator= )
   // assignment: cleanup and copy.
   //   supports:  a_Exercice = another_Exercice = yetanother_Exercice;
   Exercice& operator=(Exercice const& other);


   // assignment from other type.      Prevents: a_Exercice = a_Other;
   Exercice& operator=(Other const& other);


   //lint -restore
}
