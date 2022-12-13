import pandas as pd
from abydos.phonetic import PhoneticSpanish


def  phonetic_encoder(df,colnames):
  
  pe=PhoneticSpanish()
  
  phonetic_encoding=df[colnames].applymap(lambda x: pe.encode(x))
  
  
  return(phonetic_encoding)


