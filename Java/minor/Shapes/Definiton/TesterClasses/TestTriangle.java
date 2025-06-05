import java.awt.Color;

/**
 * Tester class for the Triangle class.
 * @author Shaheer Ziya
 */
public class TestTriangle extends TestShape {
  /**
   * Main method for testing the Square class. It initializes an instance of the
   * Square class
   * 
   * @param args Unused
   */
  public static void main(String[] args) {
    // Initialize a triangle with values for debugging
    Triangle t = new Triangle();
    t.color = Color.RED;
    t.filled = true;
    t.theta = 0;
    t.xc = 100;
    t.yc = 100;

    // Print out fields of the triangle
    printFields(t);

    // Testing the setVertices method
    System.out.println("Calling setVertices() with d = 50.");

    t.setVertices(50);

    System.out.print("The x coordinates of the vertices in the local cordinates are: ");
    printArr(t.xLocal);

    System.out.print("The y coordinates of the vertices in the local cordinates are: ");
    printArr(t.yLocal);

    System.out.println();

    // Testing the getX() and getY() methods
    System.out.print("The x coordinates of the vertices in the canvas cordinates are: ");
    printArr(t.getX());

    System.out.print("The y coordinates of the vertices in the canvas cordinates are: ");
    printArr(t.getY());

    System.out.println();

    // Test the other methods (inherited from the Shape class)
    testMethods(t);
  }
}