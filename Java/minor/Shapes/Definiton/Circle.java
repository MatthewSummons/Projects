/**
 * <p>
 * The Circle class is used to model circles. It is a subclass of the Shape
 * class and it inherits all the instance variables and methods of the Shape
 * class. 
 * </p>
 * <p>
 * The Circle class overrides the setVertices() method for setting the
 * local coordinates of the upper left and lower right vertices of an
 * axis-aligned bounding box of a standard circle, as well as the getX() and
 * getY() methods for retrieving the canvas coordinates of the upper left and
 * lower right vertices of this bounding box.
 * </p>
 */
public class Circle extends Shape {
  /**
   * A method for setting the local coordinates of the upper left and lower right 
   * vertices of an axis-aligned bounding box of a standard circle. 
   * Here, a standard circle is a circle having its center located at (0, 0) of 
   * its local coordinate system.
   * 
   * @param d Specifies the radius of the circle
   */
  public void setVertices(double d) {
    this.xLocal = new double[] { -d, d };
    this.yLocal = new double[] { -d, d };
  } // End of setVertices()

  /**
   * A method for retrieving the x-coordinates of the upper left and lower right
   * vertices of an axis-aligned bounding box of the circle in the canvas
   * coordinate system (rounded to nearest integers)
   */
  public int[] getX() {
    /* d is the radius of the circle
     * xCanvas[0] is the x-coordinate of the upper left vertex of the bounding box
     * xCanvas[1] is the x-coordinate of the lower right vertex of the bounding box
     */
    int[] xCanvas = new int[] {
       (int) Math.round(this.xLocal[0] + xc),
       (int) Math.round(this.xLocal[1] + xc)
    };
    
    return xCanvas;
  } // End of getX()

  /**
   * a method for retrieving the y-coordinates of the upper left and lower right
   * vertices of an axis-aligned bounding box of the circle in the canvas
   * coordinate system (rounded to nearest integers).
   */
  public int[] getY() {
    /* d is the radius of the circle
     * yCanvas[0] is the y-coordinate of the upper left vertex of the bounding box
     * yCanvas[1] is the y-coordinate of the lower right vertex of the bounding box
     */
    int[] yCanvas = new int[] {
      (int) Math.round(this.yLocal[0] + yc),
      (int) Math.round(this.yLocal[1] + yc)
    };
    return yCanvas;
  } // End of getY()

} // End of Circle class
