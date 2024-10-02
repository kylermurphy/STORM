
restore,'C:\data\IMAGE_FUV\processed\se_dat.sav'

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


end