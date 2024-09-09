pro image_plot, $
    im_str, $
    im_max=im_max, $
    im_min=im_min, $
    ns_scl=ns_scl, $
    clog = clog, $ 
    ct=ct, $
    win=win, $
    _EXTRA = ex
    
    fixplot
    
    if keyword_set(im_min) then im_min=im_min else im_min=im_str.ns_min
    if keyword_set(im_max) then im_max=im_max else im_max=im_str.ns_max
    if keyword_set(ns_scl) then begin
      im_min = im_str.ns_min
      im_max = im_str.ns_max
    endif
    
    if keyword_set(clog) then begin
      im_col = bytscl(alog10(im_str.im_flat),/nan, $
        min=alog10(im_min+1),max=alog10(im_max))
        im_min = im_min+1
    endif else begin
      im_col = bytscl(im_str.im_flat,/nan, $
        min=im_min,max=im_max)
    endelse
    
    if keyword_set(ct) then ct=ct else ct=56
    if keyword_set(win) then win=win else win=0
    if keyword_set(wf) then wf=wf else wf=0.02
    if keyword_set(cw) then cw=cw else cw=0.03
    
    ; rotate the figure
    ; so that 0 degerees
    ; is on the -ve y axis
    ns_d = 90
    ; minimum to plot
    r_min = im_str.colat_min
    p_arr = [-1*r_min,r_min]
    ; rotate the plot so the -ve
    ; y-axis is is zero longitude
    rot_t = 90
    
    r = 90-im_str.lat_vert
    theta = im_str.lon_vert-im_str.mlon_mid 
    ; setup the plot area
    window, win, _EXTRA=ex
    !x.margin=[5,15]
    !y.margin=[5,5]
    loadct, 0, /silent
    plot, p_arr, p_arr, /nodata, /isotropic, $
      xtickf='no_ticks', ytickf='no_ticks', $
      xticks=1, yticks=1, xminor=1, yminor=1
    loadct, ct, /silent
    for i=0, im_col.length-1 do begin
      x = r[i,*]*cos((theta[i,*]-rot_t)*!dtor)
      y = r[i,*]*sin((theta[i,*]-rot_t)*!dtor)
      
      
      polyfill, x, y, color=im_col[i]
    endfor
    
    p0 = convert_coord(!p.clip[0],!p.clip[1], /device, /to_normal)
    p1 = convert_coord(!p.clip[2],!p.clip[3], /device, /to_normal)
    
    width = (p1[0]-p0[0])*wf
    cwidth = (p1[0]-p0[0])*cw

    position = [p1[0]+width,p0[1],p1[0]+width+cwidth,p1[1]]
    colorbar_krm,position,im_min,im_max,ct, $
      /vertical, /right, c_title = 'Raylieghs', $
      log=clog, tl = -0.3, rev_ct = rev_ct, ct_file=ct_file
    
end

;todo
; change/test return arrays to a lambda
; avg = lambda(n : total(n/nan)
; val = vec.map(avg,/vector)
; add coordinates to image_bin_km 



; main
; 

fn = "D:\data\IMAGE_FUV\2001\WIC\015\wic20010150809.idl"
im0 = image_bin_ll(fn, lon_res=2, colat_min=20)
im1 = image_bin_km(fn, /ns_scl, colat_min=20) 

image_plot,im0, xsize=900, ysize=900
image_plot,im1, xsize=900, ysize=900, win=1


end