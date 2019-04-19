library("openssl")

transaction <- 'Cesare sends one bitcoin to Shimon'
hash <- sha256(transaction)

hash.prev <- '85738f8f9a7f1b04b5329c590ebcb9e425925c6d0984089c43a022de4f19c281'
time.stamp <- '2018-01-07 21:05:34'
bits <- '3'

block.head <- paste(transaction, hash, hash.prev, bits, sep = ' ')
block.head

nonce <- 0

while (substr(new.hash, start = 1, stop = 3) != '000'){
    block.temp <- paste(block.head, nonce, sep = ' ')
    new.hash <- sha256(block.temp)
    
    print(nonce)
    print(substr(new.hash, start = 1, stop = 3))
    print(new.hash)
    
    nonce = nonce + 1
}





