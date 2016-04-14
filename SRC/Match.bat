@echo off
:menu
goto MatchAccurate
pause

:MatchAccurate
time < Enter.txt
Match img1/img1.bmp img1/img1_partial.bmp 0
time < Enter.txt
Match img2/img2.bmp img2/img2_partial.bmp 0
time < Enter.txt
Match img3/img3.bmp img3/img3_partial.bmp 0
time < Enter.txt
Match img4/img4.bmp img4/img4_partial.bmp 0
time < Enter.txt
Match img5/img5.bmp img5/img5_partial.bmp 0
time < Enter.txt
goto done

:MatchLineVague
time < Enter.txt
Match img1/img1.bmp img1/img1_partial_blur.bmp 1
time < Enter.txt
Match img2/img2.bmp img2/img2_partial_blur.bmp 1
time < Enter.txt
Match img3/img3.bmp img3/img3_partial_blur.bmp 1
time < Enter.txt
Match img4/img4.bmp img4/img4_partial_blur.bmp 1
time < Enter.txt
Match img5/img5.bmp img5/img5_partial_blur.bmp 1
time < Enter.txt
goto done

:MatchColorDeepen
time < Enter.txt
Match img1/img1.bmp img1/img1_partial_color.bmp 2
time < Enter.txt
Match img2/img2.bmp img2/img2_partial_color.bmp 2
time < Enter.txt
Match img3/img3.bmp img3/img3_partial_color.bmp 2
time < Enter.txt
Match img4/img4.bmp img4/img4_partial_color.bmp 2
time < Enter.txt
Match img5/img5.bmp img5/img5_partial_color.bmp 2
time < Enter.txt
goto done

:MatchSaltPepperVague
time < Enter.txt
Match img1/img1.bmp img1/img1_partial_noise.bmp 3
time < Enter.txt
Match img2/img2.bmp img2/img2_partial_noise.bmp 3
time < Enter.txt
Match img3/img3.bmp img3/img3_partial_noise.bmp 3
time < Enter.txt
Match img4/img4.bmp img4/img4_partial_noise.bmp 3
time < Enter.txt
Match img5/img5.bmp img5/img5_partial_noise.bmp 3
time < Enter.txt
goto done

:MatchBlackPollute
time < Enter.txt
Match img1/img1.bmp img1/img1_partial_taint.bmp 4
time < Enter.txt
Match img2/img2.bmp img2/img2_partial_taint.bmp 4
time < Enter.txt
Match img3/img3.bmp img3/img3_partial_taint.bmp 4
time < Enter.txt
Match img4/img4.bmp img4/img4_partial_taint.bmp 4
time < Enter.txt
Match img5/img5.bmp img5/img5_partial_taint.bmp 4
time < Enter.txt
goto done

:done
pause