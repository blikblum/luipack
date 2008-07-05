* Info *

These packages are intended to replace the default package found in the distribution of Vampyre Imaging Library

* Rational *

The Vampyre Imaging Library has a group of units/functions to load and manipulate image data and other group of functions to interact image data with LCL components (TCanvas, TBitmap). The default package provides both groups.

Separating these group of functions in separated packages has some benefits:

- Allow other graphical toolkits like fpgui to use Vampyre Imaging
- Allow to use load/manipulating data functions in LCL widgetsets currently unsupported by Vampyre Imaging like Qt, Carbon
- Avoid the code breaking after each time LCL changes it's graphical internal interfaces

* Usage *

Place these files in [Imaging dir]/Source/Projects

Load vampyreimaging_base package
Load vampyreimagingpackage package (if the LCL functions is required)

For both packages is not necessary to click "Install" in the package dialog. Just click "Compile"

* Links * 

The Vampyre Imaging homepage: http://imaginglib.sourceforge.net/
The forun post with my feature request: http://galfar.vevb.net/imaging/smf/index.php/topic,78.0.html