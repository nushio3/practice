# set terminal pngcairo  background "#ffffff" enhanced font "arial,10" fontscale 1.0 size 512, 384
# set output 'transparent_solids.1.png'
unset border
set style fill  transparent solid 0.10 border
set dummy u,v
unset key
set style line 2  linetype 2 linecolor rgb "#a0a0f0"  linewidth 0.500 pointtype 2 pointsize default pointinterval 0
set object  1 rect from screen 0, 0, 0 to screen 1, 1, 0
set object  1 behind lw 1.0 fc  rgb "gray"  fillstyle   solid 1.00 border lt -1
set parametric
set view 64, 345, 1.24375, 0.995902
set isosamples 50, 20
set noxtics
set noytics
set noztics
set title "Interlocking Tori - PM3D surface with depth sorting and transparency"
set urange [ -3.14159 : 3.14159 ] noreverse nowriteback
set vrange [ -3.14159 : 3.14159 ] noreverse nowriteback
set pm3d depthorder
set pm3d interpolate 1,1 flush begin noftriangles hidden3d 2 corners2color mean
set palette rgbformulae 8, 9, 7
splot cos(u)+.5*cos(u)*cos(v),sin(u)+.5*sin(u)*cos(v),.5*sin(v) with pm3d,     1+cos(u)+.5*cos(u)*cos(v),.5*sin(v),sin(u)+.5*sin(u)*cos(v) with pm3d
