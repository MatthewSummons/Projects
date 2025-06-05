import java.awt.Color;

/** A class that instantiates a RegularPolygon object and tests its methods.
 * @author Shaheer Ziya
 */
public class RegularPolygonTester {
  public static void main(String[] args) {
    RegularPolygon myPolygon = new RegularPolygon();

    // Test the setters for the class ReularPolygon
    myPolygon.setColor(Color.RED);
    myPolygon.setFilled(true);
    myPolygon.setTheta(Math.PI / 4);
    myPolygon.setXc(1);
    myPolygon.setYc(1);
    myPolygon.setNumOfSides(6);
    myPolygon.setRadius(5);

    // Test the getters for the class RegularPolygon
    printFields(myPolygon);

    // Testing the canvas co-ordinates getter for the class Shape
    System.out.println("The canvas coordinates of the vertices of the shape are:");

    int[] xCanvas = myPolygon.getX();
    System.out.print("xCanvas = [");
    for (int i = 0; i < xCanvas.length; i++) {
      System.out.print(xCanvas[i] + ", ");
    }
    System.out.println("]");

    int[] yCanvas = myPolygon.getY();
    System.out.print("yCanvas = [");
    for (int i = 0; i < yCanvas.length; i++) {
      System.out.print(yCanvas[i] + ", ");
    }
    System.out.println("]");

    // Testing the other methods for the class RegularPolygon
    double x = 10;
    double y = 69;

    if (myPolygon.contains(x, y)) {
      System.out.println("The point (" + x + ", " + y + ") is contained within the polygon");
    }
    else {
      System.out.println("The point (" + x + ", " + y + ") is NOT contained within the polygon");
    }
    
    // Test with a different value
    x = 1;
    y = 1.5;
    
    if (myPolygon.contains(x, y)) {
      System.out.println("The point (" + x + ", " + y + ") is contained within the polygon");
    }
    else {
      System.out.println("The point (" + x + ", " + y + ") is NOT contained within the polygon");
    }
  }

  public static void printFields(RegularPolygon testCandidate) {

    System.out.println();
    System.out.println("Printing the fields of the Shape object:");
    System.out.println();

    System.out.println("Color = " + testCandidate.getColor());
    System.out.println("Filled = " + testCandidate.getFilled());
    System.out.println("Number of side: " + testCandidate.getNumOfSides());
    System.out.println("Radius: " + testCandidate.getRadius());
    System.out.println("Theta = " + testCandidate.getTheta());
    System.out.println("Printing center co-ordinates in the canvas coordinate system:");
    System.out.println("xc = " + testCandidate.getXc());
    System.out.println("yc = " + testCandidate.getYc());

    // Special case for final point
    System.out.println("Printing local co-ordinates in the shape coordinate system:");
    System.out.print("xLocal = [");
    for (int i = 0; i < testCandidate.getXLocal().length - 1; i++) {
      System.out.print(testCandidate.getXLocal()[i] + ", ");
    }
    System.out.println(testCandidate.getXLocal()[testCandidate.getXLocal().length - 1] + "]");

    System.out.print("yLocal = [");
    for (int i = 0; i < testCandidate.getYLocal().length - 1; i++) {
      System.out.print(testCandidate.getYLocal()[i] + ", ");
    }
    System.out.println(testCandidate.getYLocal()[testCandidate.getYLocal().length - 1] + "]");

  }


  
}
