class Rectangle implements IShape {
    private Shape base;
    private int width;
    private int height;

    // constructor
    Rectangle(int newx, int newy, int newwidth, int newheight) {
       base = new Shape(newx, newy);
       setWidth(newwidth);
       setHeight(newheight);
    }

   // accessors for the width & height
   int getWidth() { return width; }
   int getHeight() { return height; }
   void setWidth(int newwidth) { width = newwidth; }
   void setHeight(int newheight) { height = newheight; }

    // forward move messages
    public void moveTo(int newx, int newy) {
	base.moveTo(newx,newy); 
    }
    public void rMoveTo(int deltax, int deltay) { 
	base.rMoveTo(deltax,deltay); 
    }

    // draw the rectangle
    public void draw() {
	System.out.println(
	   "Drawing a Rectangle at:(" + base.getX() + ", " + base.getY() +
           "), width " + getWidth() + ", height " + getHeight());
   }
}
