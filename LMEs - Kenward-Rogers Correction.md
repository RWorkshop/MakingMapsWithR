LMEs - Kenward-Rogers Correction
===========================================================

Roy proposes Kenward-Roger correction whenever one has replicated or repeated measures data, or missing data.


Kenward-Roger approximations are not available in lme4. Douglas Bates has declined to spend effort implementing K-R correction because:
Bates is not convinced of the appropriateness of adjusting F-distribution degrees of freedom in this way, 
and doesn't think that the K-R algorithm will be feasible for the sorts of large-data problems he's interested in.
He finds the correspondence between K-R's notation and his difficult.

Referring to the "Satterthwaite and Kenward-Roger corrections" gives  the impression that these are well-known formulas and implementing
them would be a simple matter of writing a few lines of code.  I don't  think it is.  
I would be very pleased to incorporate such code if it  could be written but, as I said, I don't even know if such things are defined in the general case, let alone easy to calculate.

### KR asjustment with the **gls()**  function.

The function **gls()** does not provide the Kenward-Rogers adjustments, so the results will be somewhat different from SAS.
In both nlme and lme4, an implementation is almost certainly possible, although probably complicated and perhaps at the expense of all computational efficiency.
