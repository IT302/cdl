###AES Algorithm

Currently implements the 128-bit variant of 
[AES] (http://csrc.nist.gov/publications/fips/fips197/fips-197.pdf) though most 
of the plumbing for other variants are there. It utilizes on the fly SBox 
developed by [David Canright] (https://github.com/coruus/canright-aes-sboxes). 
There are two groups of implementation, iterative and non-iterative. 
An Iterative module is denoted by *Iter e.g **AESDecryptIter** is iterative while 
**AESDecrypt** is not . 

To run any module type `<sbt run>` and then select the module.

Note that the implementations are still work in progress and not production ready.
