import java.awt.Color;

/** A class that instantiates a Shape object and tests its methods. 
 * @author Shaheer Ziya 
 */
public class ShapeTester {
  public static void main(String[] args) {
    Shape myShape = new Shape();

    // Test the setters for the class Shape
    myShape.setColor(Color.RED);
    myShape.setFilled(true);
    myShape.setTheta(Math.PI / 4);
    myShape.setXc(1);
    myShape.setYc(1);

    // Init some values for the local coordinates of the shape
    double[] xLocal = new double[] {0, 1, 1, 0};
    myShape.setXLocal(xLocal);
    
    double[] yLocal = new double[] {0, 0, 1, 1};
    myShape.setYLocal(yLocal);


    // Test the getters for the class Shape
    printFields(myShape);
    
    // Testing the canvas co-ordinates getter for the class Shape
    System.out.println("The canvas coordinates of the vertices of the shape are:");
    
    int[] xCanvas = myShape.getX();
    System.out.print("xCanvas = [");
    for (int i = 0; i < xCanvas.length; i++) {
      System.out.print(xCanvas[i] + ", ");
    }
    System.out.println("]");

    int[] yCanvas = myShape.getY();
    System.out.print("yCanvas = [");
    for (int i = 0; i < yCanvas.length; i++) {
      System.out.print(yCanvas[i] + ", ");
    }
    System.out.println("]");
    
    // Test the remaining methods for the class Shape
    myShape.translate(1, 1);
    myShape.rotate(Math.PI / 2);
    
    printFields(myShape);

  }

  /** A methods which prints all the fields of a shape object
   * @param testCandidate The shape whose parameters are printed.
   *
   * @exception IllegalArgumentException if the shape is null.
   * @exception NulllPointerException if the xLocal or yLocal arrays are null.
   */
  public static void printFields(Shape testCandidate) {
    
    System.out.println();
    System.out.println("Printing the fields of the Shape object:");
    System.out.println();
    
    System.out.println("Color = " + testCandidate.getColor());
    System.out.println("Filled = " + testCandidate.getFilled());
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
