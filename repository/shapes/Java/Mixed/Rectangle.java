class Rectangle extends Shape implements IShape {
   private int width;
   private int height;

   // constructor
   Rectangle(int newx, int newy, int newwidth, int newheight) {
      super(newx, newy);
      setWidth(newwidth);
      setHeight(newheight);
   }

   // accessors for the width & height
   int getWidth() { return width; }
   int getHeight() { return height; }
   void setWidth(int newwidth) { width = newwidth; }
   void setHeight(int newheight) { height = newheight; }

   // draw the rectangle
   public void draw() {
      System.out.println("Drawing a Rectangle at:(" + getX() + ", " + getY() +
         "), width " + getWidth() + ", height " + getHeight());
   }
}
