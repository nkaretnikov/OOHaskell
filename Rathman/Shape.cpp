#include "Shape.h"

// constructor
Shape::Shape(int newx, int newy) {
   moveTo(newx, newy);
}

// accessors for x & y
int Shape::getX() { return x; }
int Shape::getY() { return y; }
void Shape::setX(int newx) { x = newx; }
void Shape::setY(int newy) { y = newy; }

// move the shape position
void Shape::moveTo(int newx, int newy) {
   setX(newx);
   setY(newy);
}
void Shape::rMoveTo(int deltax, int deltay) {
   moveTo(getX() + deltax, getY() + deltay);
}

