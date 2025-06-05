import java.awt.Color;

/**
 * Tester class for the Shape class. 
 * 
 * @author Shaheer Ziya
 */
public class TestShape {
  /**
   * Main method for testing the Shape class. It initializes an instance of the Shape class
   * and assigns fixed values to itss fields. Then calls all callable methods of
   * the Shape class and prints the results, with the exception of the getX() and
   * getY() methods since the xLocal and yLocal fields are not supposed to be initialized
   * with this class.
   *
   *  @param args Unused
   */
  public static void main(String[] args) {
    
    // Initialize a shape with values for debugging
    Shape s = new Shape();
    s.color = Color.RED;
    s.filled = true;
    s.theta = 0;
    s.xc = 100;
    s.yc = 100;
    
    // Print out fields of the shape
    printFields(s);

    // Testing the setVertices method
    System.out.println("Calling setVertices() with d = 50.");
    System.out.println("This method should do nothing.");

    s.setVertices(50);

    System.out.println();
    
    // Testing the various methods
    testMethods(s);

    // Note that getX & get Y cannot be tested since the xLocal and yLocal fields are not initialized

  } // End of main

  /**
   * Method to print the contents of an integer array
   * 
   * @param arr
   */
  static void printArr(int[] arr) {
    for (int i = 0; i < arr.length; i++) {
      System.out.print(arr[i] + " ");
    }
    System.out.println();
  } // End of printArr

  /**
   * Method to print the contents of a doubles array
   * 
   * @param arr
   */
  static void printArr(double[] arr) {
    for (int i = 0; i < arr.length; i++) {
      System.out.print(arr[i] + " ");
    }
    System.out.println();
  } // End of printArr

  /**
   * Prints out fields of the shape.
   */
  public static void printFields(Shape s) {
    System.out.println();
    System.out.println("Running the Tester Class for the Shape class. \n");
    System.out.println("Printing Debug Information:");
    System.out.println("(xc, yc) = " + "(" + s.xc + ", " + s.yc + ")");
    System.out.println("Angle Theta: " + s.theta);
    System.out.println("Boolean Filled (should be true) = " + s.filled);
    System.out.println();

  } // End of printFields

  /**
   * Tests the various methods of the Shape class. Call various callable methods
   *
   * @param s The shape to test
   */
  public static void testMethods(Shape s) {
    // Testing the translate method
    System.out.println("Calling translate() with dx = 10 and dy = 10.");
    System.out.println("This method should translate the shape by (10, 10).");

    s.translate(10, 10);

    System.out.println("(xc, yc) = " + "(" + s.xc + ", " + s.yc + ")");
    System.out.println();

    // Testing the rotate method
    System.out.println("Calling rotate() with dtheta = 0.5.");
    System.out.println("This method should rotate the shape by 0.5 radians.");

    s.rotate(0.5);

    System.out.println("Angle Theta: " + s.theta);
    System.out.println();
  } // End of testMethods method
  
} // End of TestShape class
