#include "Shape.h"
#include "Circle.h"
#include <iostream.h>

// constructor
Circle::Circle(int newx, int newy, int newradius): Shape(newx, newy) {
   setRadius(newradius);
}

// accessors for the radius
int Circle::getRadius() { return radius; }
void Circle::setRadius(int newradius) { radius = newradius; }

// draw the circle
void Circle::draw() {
  cout << "Drawing a Circle at:(" << getX() << "," << getY() <<
      "), radius " << getRadius() << endl;
}
