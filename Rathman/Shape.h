class Shape {

public:
   Shape(int newx, int newy);
   int getX();
   int getY();
   void setX(int newx);
   void setY(int newy);
   void moveTo(int newx, int newy);
   void rMoveTo(int deltax, int deltay);
   virtual void draw() = 0;

private:
   int x;
   int y;
};
