md = java.security.MessageDigest.getInstance("SHA-256");

md.update(input.getBytes("UTF-8"));
digest = md.digest();

return new java.math.BigInteger(1, digest).toString(16);
