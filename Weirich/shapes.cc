/* Object Orientation in C++ */

#include <iostream.h>

/* abstract interface declaration */

class Shape
{
public:
    virtual void Draw () = 0;
    virtual void MoveTo (int newx, int newy) = 0;
    virtual void RMoveTo (int dx, int dy) = 0;
};
			 
/* Class Rectangle */

class Rectangle : public Shape
{
public:
    Rectangle (int x, int y, int w, int h);
    virtual void Draw ();
    virtual void MoveTo (int newx, int newy);
    virtual void RMoveTo (int dx, int dy);
    virtual void SetWidth (int newWidth);
    virtual void SetHeight (int newHeight);

private:
    int x, y;
    int width;
    int height;
};
    
void Rectangle::Draw ()
{
    cout << "Drawing a Rectangle at (" << x << "," << y
	 << "), width " << width << ", height " << height << "\n";
};

void Rectangle::MoveTo (int newx, int newy)
{
    x = newx;
    y = newy;
}

void Rectangle::RMoveTo (int dx, int dy)
{
    x += dx;
    y += dy;
}

void Rectangle::SetWidth (int newWidth)
{
    width = newWidth;
}

void Rectangle::SetHeight (int newHeight)
{
    height = newHeight;
}

Rectangle::Rectangle (int initx, int inity, int initw, int inith)
{
    x = initx;
    y = inity;
    width = initw;
    height = inith;
}


/* Class Circle */

class Circle : public Shape
{
public:
    Circle (int initx, int inity, int initr);
    virtual void Draw ();
    virtual void MoveTo (int newx, int newy);
    virtual void RMoveTo (int dx, int dy);
    virtual void SetRadius (int newRadius);

private:
    int x, y;
    int radius;
};

void Circle::Draw ()
{
    cout << "Drawing a Circle at (" << x << "," << y
	 << "), radius " << radius << "\n";
}

void Circle::MoveTo (int newx, int newy)
{
    x = newx;
    y = newy;
}

void Circle::RMoveTo (int dx, int dy)
{
    x += dx;
    y += dy;
}

void Circle::SetRadius (int newRadius)
{
    radius = newRadius;
}

Circle::Circle (int initx, int inity, int initr)
{
    x = initx;
    y = inity;
    radius = initr;
}


/* ===================================================================
 * DoSomethingWithShape is a fuction that takes a polymorphic shape
 * and manipulates it according to its interface.
 */

void DoSomethingWithShape (Shape * s)
{
    s->Draw ();
    s->RMoveTo (100, 100);
    s->Draw ();
}


/* =================================================================== 
 * Main Program
 */

int main ()
{
    
    /* using shapes polymorphically */

    Shape * shapes[2];
    shapes[0] = new Rectangle (10, 20, 5, 6);
    shapes[1] = new Circle (15, 25, 8);

    for (int i=0; i<2; ++i) {
	DoSomethingWithShape (shapes[i]);
    }

    /* access a rectangle specific function */

    Rectangle * rect = new Rectangle (0, 0, 15, 15);
    rect->SetWidth (30);
    rect->Draw ();

    return 0;
}
