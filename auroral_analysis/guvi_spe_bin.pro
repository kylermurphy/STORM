;guvi superposed epoch analysis
; guvi_im[0,*] - first scan
; guvi_im[0,*] - next scan

; guvi_im[0,*] - number of pixels in a scan
; guvi_im[*,0] - number of scans

pro guvi_spe_bin



  fn = file_search('C:\data\IMAGE_FUV\processed\2001\01\pro*', count=fc)
  restore, fn[0]
  
  td = dblarr(fc)
  
  lat_min= 60
  arr_ind = indgen(75-34+1)+34
  arr_ind = [0,arr_ind]
  arr_ind = indgen(n_elements(guvi_im.im[*,0]))
  
  x = (90-guvi_im.lat_arr)*cos(guvi_im.lon_arr*!dtor)
  y = (90-guvi_im.lat_arr)*sin(guvi_im.lon_arr*!dtor)
  
  y_val = reform(y[*,105])
  se_dat = findgen(fc,arr_ind.length,n_elements(guvi_im.im[0,*]))
    
  for i=0L, fc-1 do begin
    restore, fn[i]
    se_dat[i,*,*] = guvi_im.im[arr_ind,*]
    td[i] = time_double(guvi_im.t)
  endfor  

  se_val = mean(se_dat, dimension=1, /nan)
  
  fixplot
  loadct,0,/silent
  window, 9, xsize=1700, ysize=700
  !p.multi=[0,2,1]
  !x.omargin=[0,20]
  
  plot, [-45,45], [-45,45], /nodata, /iso
  loadct,25,/silent
  plots, x,y, psym=sym(1), color=bytscl(alog10(se_val), min=2.5, max=3), noclip=0
  
  x_ind = indgen(n_elements(se_val[0,*]))
  loadct, 0,/silent
  plot, x_ind, se_val[0,*], /nodata, $
    xrange=[min(x_ind,max=mm,/nan),mm], $
    yrange=[min(se_val,max=mm,/nan),mm]
    
  ;plot color bar
  p1 = convert_coord(!p.clip[2],!p.clip[3], /device, /to_normal)
  xyouts, p1[0], p1[1], ' Lat:', /normal, charsize=1.25
  
  pre = '!C'
  c_arr = bytscl(indgen(arr_ind.length))
  loadct,25,/silent
  for i=0L, arr_ind.length-1 do begin
    oplot, x_ind, se_val[i,*], color=c_arr[i]
    if i mod 5 eq 0 then begin
      xyouts, p1[0], p1[1], $
        ' '+pre+string(y_val[i],format='(F0.2)'), $
        /normal, color=c_arr[i], charsize=1.25
        
        pre = '!C'+pre
    endif
  endfor
  
  ; load some omni data and match it to the image times
  ; drop times without a match
  om_1m = get_omni_1min(time_string(min(td, max=mtd)-60,tformat='YYYY-MM-DD/hh:mm:00'),$
    time_string(mtd,tformat='YYYY-MM-DD/hh:mm:00'))
  
  t_om = om_1m.t_th
  
  match, td, t_om, ind_im, ind_om, /sort, epsilon=30, count=c
  
  se_dat = se_dat[ind_im,*,*]
  td = td[ind_im]
  
  om_im = create_struct('name', 'Omni 1 min IMAGE match')
  str_tags = tag_names(om_1m)
  for i=0L, str_tags.length-1 do begin
    if str_tags[i] eq 'NAME' then continue
  
    x = om_1m.(i)
    x = x[ind_om]
  
    om_im = create_struct(om_im, str_tags[i], x)
  endfor
  
  
  save, se_dat, td, om_im, filename='C:\data\IMAGE_FUV\processed\se_dat.sav',/compres


  stop
end