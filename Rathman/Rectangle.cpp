#include "Shape.h"
#include "Rectangle.h"
#include <iostream.h>

// constructor
Rectangle::Rectangle(int newx, int newy, int newwidth, int newheight): Shape(newx, newy) {
   setWidth(newwidth);
   setHeight(newheight);
}

// accessors for width and height
int Rectangle::getWidth() { return width; }
int Rectangle::getHeight() { return height; }
void Rectangle::setWidth(int newwidth) { width = newwidth; }
void Rectangle::setHeight(int newheight) { height = newheight; }

// draw the rectangle
void Rectangle::draw() {
   cout << "Drawing a Rectangle at:(" << getX() << "," << getY() <<
      "), width " << getWidth() << ", height " << getHeight() << endl;
}
