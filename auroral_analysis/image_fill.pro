; identify the percent of an image filled
; when binning
; this is used for subsequent analysis so that
; we only look at image with good coverage

pro image_fill



  out_f = 'D:\data\IMAGE_FUV\image_fill.txt'
  fn = file_search('D:\data\IMAGE_FUV\2001\WIC\*\*.idl')
  pc = fltarr(fn.length)
  
  openw,outlun,out_f,/get_lun, /append
  
  t0 = systime(/seconds)
  for i=0L, fn.length-1 do begin
    im = image_bin_km(fn[i],clog=1,save_png=1)
    if size(im,/type) ne 8 then continue
    gd = where(im.im_flat gt 0,c)
    pc[i] = 1.0*c/n_elements(im.im_flat)
    printf, outlun, fn[i]+', '+string(pc[i],format='(F0.2)')
    
    if i mod 100 eq 0 then begin
      t1 = systime(/seconds)
      perc = 100*float(i)/float(fn.length)
      dur = (t1-t0)/60.
      print, 'Processed: '+string(perc,format='(F0.2)')
      print, 'Total duration: '+string(dur,format='(F0.2)')
      
    endif
  endfor

  close, outlun
  free_lun, outlun



end