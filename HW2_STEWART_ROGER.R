#install.packages("openssl")
library("openssl")

p = bignum("112481050639317229656723018120659623829736571015511322021617837187076258724819")
q = bignum("89185111938335771293328323333111422985697062149139368049232365065924632677343")

e = bignum("65537")

d = bignum_mod_inv(e, (p-1)*(q-1))
print(d)

m= bignum(charToRaw("Running late. Wait for me."))
m
n = p*q

c= bignum_mod_exp(m,e,n)
print(c)
c_enc=base64_encode(c)
print(c_enc)

new_cenc64 = 'rGhkBLUmPQStyYGrhIcNxnhZw6GeGoFGswZuUihd+kPx21VtPSMmdBRQOkKw8uLPhsh0NV4qk27G/EFuVT2iAw=='
new_cenc = bignum(base64_decode(new_cenc64))
new_cenc
m3= bignum_mod_exp(new_cenc,d,n)
m3_char=rawToChar(m3)
m3_char

m = 'Congrats! You just decrypted your first message!'
m_hash = sha256(m)
m_hash = bignum(charToRaw(m_hash))
m_hash

sig = bignum_mod_exp(m_hash,d,n)
d_sig = bignum_mod_exp(sig,e,n)

d_sig
m_hash

d_sig == m_hash

