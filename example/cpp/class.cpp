class WonderingExample
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
   //                      - WonderingExample value;
   //                      - WonderingExample arr[10];
   //                      - WonderingExample* pt = new WonderingExample[20];
   WonderingExample();

   // WonderingExample destructor: cleanup
   virtual ~WonderingExample();


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
   //lint -esym(1526, WonderingExample::WonderingExample )
   //lint -esym(1704, WonderingExample::WonderingExample )
   //lint -esym(1714, WonderingExample::WonderingExample )
   // copy constructor.
   //   supports : WonderingExample a_WonderingExample = another_WonderingExample;
   //   and      : some_function(WonderingExample value)
   WonderingExample(WonderingExample const& other);

   //lint -esym(1526, WonderingExample::operator= )
   //lint -esym(1704, WonderingExample::operator= )
   //lint -esym(1714, WonderingExample::operator= )
   // assignment: cleanup and copy.
   //   supports:  a_WonderingExample = another_WonderingExample = yetanother_WonderingExample;
   WonderingExample& operator=(WonderingExample const& other);


   // assignment from other type.      Prevents: a_WonderingExample = a_Other;
   WonderingExample& operator=(Other const& other);


   //lint -restore
}
