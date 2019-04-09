# tips and tricks
1. making scripts globally executable on mac
   ```
   sudo ln -s <your/path/to/file/here.py> /usr/local/bin
   ```
2. using [gphoto2](http://gphoto.org/) to capture images from command line eg.
   ```
   gphoto2 --capture-image-and-download --filename "%Y%m%d%H%M%S.jpg" -F 2 -I 10
   ```
3. converting row number to alphabet in excel
   ```
   =MID("ABCDEFGHIJKLMNOPQRSTUVWXYZ",(cell value),1)
   ```
