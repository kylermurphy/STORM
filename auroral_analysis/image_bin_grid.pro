;pass a grid, bin on that grid

function image_bin_grid, $
  fn, $ ; file to load
  grid, $ ; grid to bin image into
  mag = mag, $ ; use geomagnetic coordinates
  geo = geo, $ ; use geographic coordinates
  mlt = mlt $ ; use mlt and magnetic latitude
  
  if keyword_set(mag) then mag = 1 else mag = 0
  if keyword_set(geo) then geo = 1 else geo = 0
  if keyword_set(mlt) then mlt = 1 else mlt = 0
  
  ;open file
  restore, fn
  td = time_double(imageinfo.epoch, /epoch)
  ts = time_string(td)
  
  im = imageinfo.image

  if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
    coord = 'geomagnetic'
  endif else if geo eq 1 then begin
    im_lat = imageinfo.glat
    im_lon = imageinfo.glon
    coord = 'geographic'
  endif else if mag eq 1 then begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlt * 15
    coord = 'geomagnetic/mlt (mlt converted to 0-360 degrees mlt*15'
  endif else begin
    im_lat = imageinfo.mlat
    im_lon = imageinfo.mlon
    coord = 'geomagnetic'
  endelse