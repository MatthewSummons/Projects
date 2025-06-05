
/**
 * <p>
 * The Square class is used to model squares.
 * It is a subclass of the Shape class and it inherits all the instance
 * variables and methods of the Shape class.
 * </p>
 * 
 * <p>
 * The Square class overrides the setVertices() method for setting the local
 * coordinates of the 4 vertices of a standard square.
 * </p>
 * 
 * @author Shaheer Ziya
 * 
 */
public class Square extends Shape{

  // Overriding the setVertices() method
  /**
   * A method for setting the local coordinates of the 4 vertices of a standard
   * square. Here, a standard square has its center located at (0, 0) and its
   * sides being parallel to the x and y-axes of its local coordinate system.
   * 
   * @param d Specifies half the length of a side of the square
   */
  public void setVertices(double d) {
    this.xLocal = new double[] { d, d, -d, -d };
    this.yLocal = new double[] { d, -d, -d, d };
  } // End of setVertices()  

} // End of Square class
