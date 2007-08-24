class Polymorph {
   public static void main(String argv[]) {

      // create some shape instances
      IShape scribble[] = new IShape[2];
      scribble[0] = new Rectangle(10, 20, 5, 6);
      scribble[1] = new Circle(15, 25, 8);

      // iterate through the list and handle shapes polymorphically
      for (int i = 0; i < scribble.length; i++) {
         scribble[i].draw();
         scribble[i].rMoveTo(100, 100);
         scribble[i].draw();
      }

      // call a rectangle specific function
      Rectangle arect = new Rectangle(0, 0, 15, 15);
      arect.setWidth(30);
      arect.draw();
   }
}
