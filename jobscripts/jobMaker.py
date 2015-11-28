#!/usr/bin/env python
"""
Automated Job making
Because typing if for scrubs
"""

def main():
    interact()
    return

def interact():
    input = raw_input('Range or Fixed? ([0],1): ')
    if input == '1':
        specific()
    else:
        ranged()

def ranged():
    from subprocess import call
    pstart = raw_input('Pressure start: ')
    pstop = raw_input('Pressure stop: ')
    if pstart != pstop:
        pinc = raw_input('Increment?: ')
    else:
        pinc = '0'
    pst, psp, pi, pn =  checkinc(pstart,pstop,pinc)
    # xdim ranges
    xstart = raw_input('xdim start: ')
    xstop = raw_input('xdim stop: ')
    if xstart != xstop:
        xinc = raw_input('Increment?: ')
    else:
        xinc = '0'
    xst, xsp, xi, xn =  checkinc(xstart,xstop,xinc)
    # ydim ranges
    ystart = raw_input('ydim start: ')
    ystop = raw_input('ydim stop: ')
    if ystart != ystop:
        yinc = raw_input('Increment?: ')
    else:
        yinc = '0'
    yst, ysp, yi, yn =  checkinc(ystart,ystop,yinc)
    print "making " + str(yn*xn*pn) + " jobs"
    p = pstart
    x = xstart
    y = ystart
    for i in xrange(0,pn):
        for j in xrange(0,xn):
            for k in xrange(0,yn):
#when we get the formula                t, m = predictResources(p,x,y)
		t = '00:40:00'
		m = '100GB'
                fn = modscript(p,x,y,t,m)
		call(['qsub',fn])
                if k != (yn - 1):
                    y = str(int(y)+yi)
            if j != (xn - 1):
                x = str(int(x)+xi)
        if i != (pn - 1):
            p = str(float(p) + yi)
    return




def checkinc(v1,v2,inc):
    try:
        st = int(v1)
        sp = int(v2)
        i = int(inc)
    except ValueError:
        st = float(v1)
        sp = float(v2)
        i = float(inc)
    vrange = sp - st
    if i == 0:
        return (st,sp,i,1)
    elif vrange % i != 0:
        print "warning missing last " + str(vrange % i)
    return (st,sp,i,int(vrange // i))

def specific():
    from subprocess import call
    pA = raw_input('Specify pA: ')
    xdim = raw_input('Specify xdim: ')
    ydim = raw_input('Specify ydim: ')
    time, mem = predictResources(pA,xdim,ydim)
    yorn = raw_input('Are these resources ok?\n'
                     + 'Time: ' + time + '\n'
                     + 'Memory: ' + mem + '([y]/n)')
    if yorn == 'n':
        time = raw_input('Time (hh:mm:ss): ')
        mem = raw_input('Mem: ' ) + 'GB'
    fn = modscript(pA,xdim,ydim,time,mem)
    #return call(['qsub',fn])
    return

def modscript(p,x,y,t,m):
    import re
    import os
    trep = 'HH:MM:00'
    mrep = 'MEMORYLIMIT'
    prep = 'PRESSURE'
    xrep = 'XDIM'
    yrep = 'YDIM'
    f = open('testscript.sh','r')
    base = f.read()
    f.close()
    dir = 'autogened'
    if not os.path.exists(dir):
        os.makedirs(dir)
    mod = base.replace(trep,t,1)
    mod = mod.replace(mrep,m,1)
    mod = mod.replace(prep,p,1)
    mod = mod.replace(xrep,x,1)
    mod = mod.replace(yrep,y,1)
    fn = 'P:' + p + 'X:' + x + 'Y:' + y + '.sh'
    f = open(dir+'/'+fn,'w')
    f.write(mod)
    f.close()
    return dir+'/'+fn

def predictResources(p,x,y):
    pres = float(p)
    xd = int(x)
    yd = int(y)
    seconds = 0
    mem = 0
    m, s = divmod(seconds, 60)
    h, m = divmod(m, 60)
    time = "%d:%02d:%02d" % (h,m,s)
    mem = str(mem)+'GB'
    return (time,mem)

if __name__ == '__main__':
    main()
