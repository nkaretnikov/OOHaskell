#include <iostream.h>
#include "Shape.h"
#include "Circle.h"
#include "Rectangle.h"

int main(void) {
   // set up array of shapes
   Shape *scribble[2];
   scribble[0] = new Rectangle(10, 20, 5, 6);
   scribble[1] = new Circle(15, 25, 8);

   // iterate through the array
   // and handle shapes polymorphically
   for (int i = 0; i < 2; i++) {
      scribble[i]->draw();
      scribble[i]->rMoveTo(100, 100);
      scribble[i]->draw();
   }

   // call a rectangle specific function
   Rectangle *arec = new Rectangle(0, 0, 15, 15);
   arec->setWidth(30);
   arec->draw();
}
