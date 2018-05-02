VISCfunctions
=============

A collection of VISC functions for annotation, statistical calculations, data manipulation, etc.

The first rule of VISCfunctions: **We NEVER push to the master.**    
Please use [pull requests](https://github.fhcrc.org/VIDD-VISC/SRA-Documentation/blob/master/Code-Documentation/pull-request-information.md).    
[Click here for a brief summary on building and installing a package with RStudio.](https://github.fhcrc.org/VIDD-VISC/SRA-Documentation/blob/master/Code-Documentation/build_and_test_package.md)


VISCfunctions Principles
=============

1. Documentation
  * Create thorough and clear help files for your functions. Useful examples will motivate the use of your function. 
  * Code should be [readable](http://adv-r.had.co.nz/Style.html) and include **comments**.
  * Don't pack too much into one function. Multiple tasks may require multiple functions (modularity).
  * Function, arguments, and variables should have intuitive, concise names. This is not easy! 

2. Teamwork and Communication
 * Seek code review and feedback.
 * Be a mindful contributer.
 * Think about modularity. If everyone is writing similar code in their functions, there may be room for an internal,  shared function.
 * Share your ideas and plans!

3. Testing - Error Control
 * Write tests!
 * Take time to consider unexpected user errors. Discuss your function with others.
 * If your function utilizes other functions, make sure there are not new opportunities for bugs.
 * Once the testing framework is implemented (Version 1 release), the package should compile error free.
 * Modularity of functions greatly aids in error checking and control.

4. Version Control and Stability
 * Utilize the features of git. Use branches to test new code and and ideas without pushing to the master.
 * **Never git force push**
 * Your changes may affect previously written reports (Post version 1 release): be mindful!

![Image of Yaktocat](https://github.fhcrc.org/VIDD-VISC/VISCfunctions/blob/master/Viscfunction_diagram.png)
