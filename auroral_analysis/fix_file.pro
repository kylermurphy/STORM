; stored the angle wrong....

pro fix_file

  fn = file_search('C:\data\IMAGE_FUV\processed\2001\01\pro*', count=fc)
  
  for i=0L, fc-1 do begin
    restore, fn[i]
    a = 90-guvi_im.lon_arr
    guvi_im.lon_arr = a
    save, guvi_im, ll_im, filename=fn[i], /compress
  endfor
end