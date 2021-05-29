// Class wandering_one: Example of the class name typed as wandering-one, with a dash, and with a comment block.
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

class wandering_one
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
   //                      - wandering_one value;
   //                      - wandering_one arr[10];
   //                      - wandering_one* pt = new wandering_one[20];
   wandering_one();

   // wandering_one destructor: cleanup
   virtual ~wandering_one();


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
   //lint -esym(1526, wandering_one::wandering_one )
   //lint -esym(1704, wandering_one::wandering_one )
   //lint -esym(1714, wandering_one::wandering_one )
   // copy constructor.
   //   supports : wandering_one a_wandering_one = another_wandering_one;
   //   and      : some_function(wandering_one value)
   wandering_one(wandering_one const& other);

   //lint -esym(1526, wandering_one::operator= )
   //lint -esym(1704, wandering_one::operator= )
   //lint -esym(1714, wandering_one::operator= )
   // assignment: cleanup and copy.
   //   supports:  a_wandering_one = another_wandering_one = yetanother_wandering_one;
   wandering_one& operator=(wandering_one const& other);


   // assignment from other type.      Prevents: a_wandering_one = a_Other;
   wandering_one& operator=(Other const& other);


   //lint -restore
}
