/** The RegularPolygon class is a subclass of the Shape class and is used to
 * model regular n- sided polygons. Besides the properties it inherited from the
 * Shape class, the RegularPolygon class also declares a number of private
 * instance variables for storing the number of sides and the radius of a
 * polygon. It has public getters and setters for accessing its private instance
 * variables. It also has public methods for setting the local coordinates of
 * the vertices of a polygon and for checking if a point (in the canvas
 * coordinate system) is contained by a polygon.
 * 
 * @author Shaheer Ziya
 */
public class RegularPolygon extends Shape {

  /** A constructor for building a regular n-sided polygon with a radius of r.
   * (Note that if the argument n is less than 3, the number of sides will be set
   * to 3; if the argument r is less than 0, the radius will be set to 0).
   * @param n The number of sides for the regular polygon
   * @param r The radius of the circle that circumscribes the regular polygon
   */
  public RegularPolygon(int n, double r) {
    // Ensure that the number of sides is at least 3 and the radius is at least 0
    if (n < 3) {n = 3;}
    if (r < 0) {r = 0;}

    // Set the number of sides and the radius
    this.numOfSides = n;
    this.radius = r;
    this.setVertices();
  }

  /** A constructor for building a regular n-sided polygon with a radius of 1.0.
   * If the argument n is less than 3, the number of sides will be set to 3.
   * @param n The number of sides for the regular polygon
   */
  public RegularPolygon(int n) {
    if (n < 3) {n = 3;}
    this.numOfSides = n;
    this.radius = 1.0;
    this.setVertices();
  }

  /** A no argument constructor for RegularPolygon which asigns it 3 sides and a radius of 1 by default */
  public RegularPolygon() {
    this.numOfSides = 3;
    this.radius = 1.0;
    this.setVertices();
  }

  // Class Fields //

  /** An integer value specifying the number of sides of the regular n-sided polygon. */
  private int numOfSides;
  /** A double value specifying the radius of the regular n-sided polygon. */
  private double radius;

  // Class Methods //

  /** A method for retrieving the number of sides of the regular polygon.
   * @return The number of sides of the regular polygon
   */
  public int getNumOfSides() {
    return this.numOfSides;
  }

  /** A method for retrieving the radius of the regular polygon.
   * @return The radius of the regular polygon
   */
  public double getRadius() {
    return this.radius;
  }

  /** A method for setting the number of sides of the regular n-sided polygon. This method will also 
   * reset the local coordinates of the vertices of the regular n-sided polygon. 
   * (Note that if the argument n is less than 3, the number of sides will be set to 3).
   * @param n The number of sides for the regular polygon
   */
  public void setNumOfSides(int n) {
    if (n < 3) {n = 3;}
    this.numOfSides = n;
    this.setVertices();
  }

  /** A method for setting the radius of the regular n-sided polygon. This method should will reset the 
   * local coordinates of the vertices of the regular n-sided polygon.
   * (Note that if the argument r is less than 0, the radius will be set to 0).
   * @param r The radius of the circle that circumscribes the regular polygon
   */
  public void setRadius(double r) {
    if (r < 0) {r = 0;}
    this.radius = r;
    this.setVertices();
  }

  /** A method for setting the local coordinates of the vertices of the regular
   * n-sided polygon based on its number of sides and radius (see appendix of A2). If
   * the number of sides is an odd number, the first vertex should lie on the
   * positive x-axis and its distance from the origin is given by the radius of
   * the regular n-sided polygon. The rest of the vertices can be obtained by
   * rotating this vertex about the origin by a multiple of 2pi/n, where n is the
   * number of sides, in a counter-clockwise manner. If the number of sides is an even number, 
   * the first vertex should lie in the first quadrant (i.e., both its x and y coordinates being 
   * positive) and make an angle of pi/n, where n is the number of sides, with the positive x-axis. 
   * Its distance from the origin is again given by the radius of the regular n-sided polygon. 
   * Similarly, the rest of the vertices can be obtained by rotating this vertex about the origin 
   * by a multiple of 2pi/n, where n is the number of sides, in a counter-clockwise manner.
   */
  public void setVertices() {
    // Decide the value of alpha based on the parity of numOfSides
    // Alpha decides whether the shape's first vertex is on the x-axis or not
    double alpha = 0;   // If numOfSides is odd
    if (this.getNumOfSides() % 2 == 0) {alpha = Math.PI / this.getNumOfSides();}

    double theta = (2 * Math.PI) / this.getNumOfSides();

    double[] xLocal = new double[this.getNumOfSides()];
    double[] yLocal = new double[this.getNumOfSides()];

    // Calculate the local coordinates of the vertices of the regular polygon
    for (int i = 0; i < this.getNumOfSides(); i++) {
      xLocal[i] = this.getRadius() * Math.cos(alpha - (i * theta));
      yLocal[i] = this.getRadius() * Math.sin(alpha - (i * theta));
    }

    // Update the local coordinates of the vertices of the regular polygon
    this.setXLocal(xLocal);
    this.setYLocal(yLocal);
  }

  
  /** A method for determining if a point (x, y) in the screen coordinate system is
   * contained by the regular n-sided polygon. A point is considered to be
   * contained by a polygon if it lies either completely inside the polygon, or on
   * any of the sides or vertices of the polygon
   * 
   * @param xCanvas The x-coordinate of the point in the screen coordinate system
   * @param yCanvas The y-coordinate of the point in the screen coordinate system
   * @return true if the point is contained by the regular n-sided polygon, false otherwise
   */
  public boolean contains(double xCanvas, double yCanvas) {
    
    // Convert the point from screen coordinates to local coordinates
    double x = ((xCanvas - this.getXc()) * Math.cos(-this.getTheta())) - 
        ((yCanvas - this.getYc()) * Math.sin(-this.getTheta()));
    
    double y = ((xCanvas - this.getXc()) * Math.sin(-this.getTheta())) +
        ((yCanvas - this.getYc()) * Math.cos(-this.getTheta()));

    
    // Note that the floor(n/2 + 1) is the index of a vertex on the leftmost vertical edge
    int leftmostVertexIndex = (int) Math.floor(this.getNumOfSides() / 2);
    
    /* Find the coordinates of the leftmost vertex; We need only consider a single x co-ordinate
     * because the polygon is symmetric about the y-axis and has rotational symmetry about the
     * origin. */
    double xPolygon = this.getXLocal()[leftmostVertexIndex];
    
    // The angle of each rotation
    double theta = 2 * (Math.PI / this.getNumOfSides());
    
    /* The RegularPolygons will always be oriented such that there is a leftmost vertical edge
     * Check if the point is to the left of the leftmost vertical line. If it is, then the point is not
     * contained by the RegularPolygon. Otherwise, rotate the RegularPolygon and the point, then
     * check again. Repeat this a total of (numOfSides - 1) times. If still not contained, then the point
     * is not contained by the RegularPolygon.    */
    for (int i = 0; i < this.getNumOfSides(); i++) {
      // Calculate the rotated points (Only need to consider the x-coordinates)
      double xRotated = (x * Math.cos(i * theta)) - (y * Math.sin(i * theta));
      // double uXRotated = (uX * Math.cos(i * this.getTheta())) - (uY * Math.sin(i * this.getTheta()));
      
      // Check if the point is to the left of the leftmost vertical line, if so, return false
      if (xRotated < xPolygon) {return false;}
    }

    // If passed through every iteration of the for loop, then the point is contained by the RegularPolygon
    return true;
  }

}
