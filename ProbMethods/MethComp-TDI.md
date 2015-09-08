MethComp Package - Compute Lin's Total deviation index

### Description

This index calculates a value such that a certain fraction of difference between methods will be numerically smaller than this.

### Usage

<pre><code>
TDI( y1, y2, p = 0.05, boot = 1000, alpha = 0.05 )
</code></pre>

### Arguments

* y1	- Measurements by one method.
* y2	- Measurements by the other method
* p	- The fraction of items with differences numerically exceeding the TDI
* boot	- If numerical, this is the number of bootstraps. If FALSE no confidence interval for the TDI is produced.
* alpha	 - 1 - confidence degree.
