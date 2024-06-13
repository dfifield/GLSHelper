function r = polyint(p, a, b)
%POLYINT Integrate polynomial.
%
%   R = POLYINT(P) integrates the polynomial P.
%   I = POLYINT(P, T) integrates the polynomial from 0 to T.
%   I = POLYINT(P, A, B) integrates the polynomial from A to B.
%
%   P is a vector of coefficients in decreasing order.
%
%   See also POLYDER, POLYNDER, POLYNINT.

%   Author:      Peter J. Acklam
%   Time-stamp:  2004-02-03 22:12:45 +0100
%   E-mail:      pjacklam@online.no
%   URL:         http://home.online.no/~pjacklam

   % Check number of input arguments.
   nargsin = nargin;
   error(nargchk(1, 3, nargsin));

   % Check array class.
   if ~isnumeric(p)
      error('P must be a numeric array.');
   end

   % Check array size.
   if (ndims(p) ~= 2) | (size(p, 1) ~= 1)
      error('P must be row vector.');
   end

   % Get number of coefficients and perform integration.
   n = length(p);
   r = [ p./(n:-1:1), 0 ];

   % Evaluate integral.
   if (nargsin == 2)                    % POLYINT(P, T)
      r = polyval(r, a);
   elseif (nargsin == 3)                % POLYINT(P, A, B)
      r = polyval(r, b) - polyval(r, a);
   end
