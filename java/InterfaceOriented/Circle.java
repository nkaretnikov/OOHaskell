class Circle implements IShape {
    private Shape base;
    private int radius;

    // constructor
    Circle(int newx, int newy, int newradius) {
	base = new Shape(newx,newy);
	setRadius(newradius);
    }

    // accessors for the radius
    int getRadius() { return radius; }
    void setRadius(int newradius) { radius = newradius; }

    // forward move messages
    public void moveTo(int newx, int newy) {
	base.moveTo(newx,newy); 
    }
    public void rMoveTo(int deltax, int deltay) { 
	base.rMoveTo(deltax,deltay); 
    }

    // draw the circle
    public void draw() {
	System.out.println(
	   "Drawing a Circle at:(" + base.getX() + ", " + base.getY() +
           "), radius " + getRadius());
    }
}

interface ICircle extends IShape {
    int getRadius();
    void setRadius(int newradius);
}
