abstract class Shape {
   private int x;
   private int y;

   // constructor
   Shape(int newx, int newy) {
      moveTo(newx, newy);
   }

   // accessors for x & y
   int getX() { return x; }
   int getY() { return y; }
   void setX(int newx) { x = newx; }
   void setY(int newy) { y = newy; }

   // move the x & y position
   public void moveTo(int newx, int newy) {
      setX(newx);
      setY(newy);
   }
   public void rMoveTo(int deltax, int deltay) {
      moveTo(getX() + deltax, getY() + deltay);
   }
}

interface IShape {
    void moveTo(int newx, int newy);
    void rMoveTo(int deltax, int deltay);
    void draw();
}
