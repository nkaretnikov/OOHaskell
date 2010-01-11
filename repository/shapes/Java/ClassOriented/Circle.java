class Circle extends Shape {
   private int radius;

   // constructor
   Circle(int newx, int newy, int newradius) {
      super(newx, newy);
      setRadius(newradius);
   }

   // accessors for the radius
   int getRadius() { return radius; }
   void setRadius(int newradius) { radius = newradius; }

   // draw the circle
   void draw() {
      System.out.println(
         "Drawing a Circle at:(" + getX() + ", " + getY() +
         "), radius " + getRadius());
   }
}

