import java.awt.Color;

/**
 * The Shape class is an abstract class used to model general shapes. 
 * It has instance variables to store the color, filled status, orientation, canvas
 * coordinates of the center, and local coordinates of the vertices of the shape.
 * It has methods for getting the canvas coordinates of the vertices of a shape.
 *
 * @author Shaheer Ziya
 * 
 */
public class Shape {
  /** Color of the Shape */
  public Color color;                  
  /** Whether the shape is filled or not (with color) */
  public boolean filled;
  /** Orientation of the shape (in radians) in the canvas cordinates system */
  public double theta; 
  /** Canvas coordinates of the center of the shape */
  public double xc, yc;
  /** Local coordinates of the vertices of the shape ( in anticlockwise order) */
  public double xLocal[], yLocal[];


  /** A method for setting the local corrdinates of the vertices of the shape.
   *  This is a dummy method and should be overridden by the subclasses
   * @param d Ununsed parameter
   *
   */
  public void setVertices(double d) {
  }


  /**
   * A method for translating the center of th shape by dx and dy respectively
   * along the x and y axes of the canvas coordinate system
   * @param dx The amount of translation along the x axis
   * @param dy The amount of translation along the y axis
   */
  public void translate(double dx, double dy) {
    xc += dx;
    yc += dy;
  } // End of translate()


  /**
   * A method for rotating the shape by dtheta radians about the center of the shape
   * @param dtheta The amount of rotation in radians
   */
  public void rotate(double dtheta) {
    theta += dtheta;
  } // End of rotate()


  /**
   * A method for getting the x canvas coordinates of the vertices of the shape
   * (in counter clockwise order rounded to the nearest integer).
   * 
   * We use the following formula to get the canvas coordinates of the vertices
   * from the local coordinates of the vertices
   * x' = (x*cos(𝛉)) - (y*sin(𝛉)) + xc
   * 
   * @return An array of integers containing the x canvas coordinates of the vertices
   */
  public int[] getX() {
    int[] xCanvas = new int[xLocal.length];
   
    for (int i = 0; i < xLocal.length; i++) {
      // Math.round return long
      xCanvas[i] = (int) Math.round(((xLocal[i] * Math.cos(theta)) - (yLocal[i] * Math.sin(theta)) + xc));
    } // End of for loop
    
    return xCanvas; 
  } // End of getX()

  
  /**
   * A method for getting the y canvas coordinates of the vertices of the shape
   * (in counter clockwise order rounded to the nearest integer).
   * 
   * We use the following formula to get the canvas coordinates of the vertices
   * from the local coordinates of the vertices
   * y' = (x*sin(𝛉)) + (y*cos(𝛉)) + yc
   * 
   * @return An array of integers containing the y canvas coordinates of the
   *         vertices
   */
  public int[] getY() {
    int[] yCanvas = new int[yLocal.length];
    
    for (int i = 0; i < yLocal.length; i++) {
      // Math.round return long
      yCanvas[i] = (int) Math.round(((xLocal[i] * Math.sin(theta)) + (yLocal[i] * Math.cos(theta)) + yc));
    } // End of for loop
    
    return yCanvas;
  } // End of getY()

} // End of Shape class