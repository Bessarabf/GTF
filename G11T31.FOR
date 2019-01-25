      function g11t31(f,t)
      double precision a,b,c,e,ps,r,s,u,v
      data s/1.976573d-1/,c/9.802712d-1/
      data ps/1.74532925199432d-2/
      e=dble(t)*ps
      u=dble(f)*ps
      a=dcos(e)
      b=dcos(u)
      r=a*b*s
      a=dsin(e)
      v=c*a
      e=v+r
      a=dsin(u)
      r=s*a
      b=datan2(r,e)
      g11t31=sngl(b)
      return
      end
