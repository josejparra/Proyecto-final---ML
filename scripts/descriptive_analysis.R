library(tidyverse)
library(janitor)
library(kableExtra)


# DESCRIPTIVAS CENSOS -----------------------------------------------------


# Frecuencia Apellidos y Nombres ------------------------------------------

freq_n1_1859 <- census_1859 %>%
  select(name_1) %>% 
  tabyl(name_1) %>% 
  arrange(desc(n)) %>% 
  select(name_1,percent
         ) %>% 
  mutate(
    percent=round(percent,2)
    
  )

freq_s1_1859 <- census_1859 %>%
  select(surname_1) %>% 
  tabyl(surname_1) %>% 
  arrange(desc(n)) %>% 
  select(surname_1,percent)%>% 
  mutate(
    percent=round(percent,2)
    
  )


freq_n1_1869 <- census_1869 %>%
  select(name_1) %>% 
  tabyl(name_1) %>% 
  arrange(desc(n)) %>% 
  select(name_1,percent)%>% 
  mutate(
    percent=round(percent,2)
    
  )

freq_s1_1869 <- census_1869 %>%
  select(surname_1) %>% 
  tabyl(surname_1) %>% 
  arrange(desc(n)) %>% 
  select(surname_1,percent)%>% 
  mutate(
    percent=round(percent,2)
    
  )




tibble(
  "Censo"=c("1859",rep("",4),"1869",rep("",4)),
  
  "Nombre/Apellido"=c(freq_n1_1859$name_1[1:5],
                      freq_n1_1869$name_1[1:5]
                      ),
  
  "Porcentaje"=c(freq_n1_1859$percent[1:5],
              freq_n1_1869$percent[1:5]
  )
  
  
) %>% 
  kable(.,escape = F,format="latex", booktabs=T) %>% 
  kable_styling()
  




tibble(
  "Censo"=c("1859",rep("",4),"1869",rep("",4)),
  
  "Nombre/Apellido"=c(freq_s1_1859$surname_1[1:5],
                      freq_s1_1869$surname_1[1:5]
  ),
  
  "Porcentaje"=c(freq_s1_1859$percent[1:5],
                 freq_s1_1869$percent[1:5])
  )%>% 
    kable(.,escape = F,format="latex", booktabs=T) %>% 
    kable_styling()
  
  
  


# Distribución de edades por géneros --------------------------------------



census_1859_binsage <- census_1859 %>% 
  mutate(
    bins_age=
      case_when(
        age>=0&age<10~"[0, 10)",
        age>=10&age<20~"[10, 20)",
        age>=20&age<30~"[20, 30)",
        age>=30&age<40~"[30, 40)",
        age>=40&age<50~"[40, 50)",
        age>=50&age<60~"[50, 60)",
        age>=60&age<70~"[60, 70)",
        age>=70&age<80~"[50, 60)",
        age>=80&age<90~"[80, 90)",
        age>=90&age<100~"[90, 100)",
        age>=100~">100"
      )
    
  ) %>% 
  filter(!is.na(age))

  ggplot(census_1859_binsage,aes(x=bins_age,fill=gender)) + 
  geom_bar(data=subset(census_1859_binsage,gender=="f"),width = 1) + 
  geom_bar(data=subset(census_1859_binsage,gender=="m"),aes(y=..count..*(-1)),width=1) + 
  scale_y_continuous(breaks=seq(-1200,1200,200),labels=abs(seq(-1200,1200,200))) + 
#  scale_x_discrete(breaks=seq(0,90,10))+
  ylab("")+
  xlab("Edad")+
  scale_fill_discrete(name="",labels=c("F","M"))+
  coord_flip()


ggsave("piramid_1859.png",dpi="retina",width=8,height=5)


census_1869_binsage <- census_1869 %>% 
  mutate(
    age=as.numeric(age),
    bins_age=
      case_when(
        age>=0&age<10~"[0, 10)",
        age>=10&age<20~"[10, 20)",
        age>=20&age<30~"[20, 30)",
        age>=30&age<40~"[30, 40)",
        age>=40&age<50~"[40, 50)",
        age>=50&age<60~"[50, 60)",
        age>=60&age<70~"[60, 70)",
        age>=70&age<80~"[50, 60)",
        age>=80&age<90~"[80, 90)",
        age>=90&age<100~"[90, 100)"
#        age>=100~"[100, Inf)"
        
        
        
      )
    
  ) %>% 
  filter(!is.na(age))

ggplot(census_1869_binsage,aes(x=bins_age,fill=gender)) + 
  geom_bar(data=subset(census_1869_binsage,gender=="F"),width = 1) + 
  geom_bar(data=subset(census_1869_binsage,gender=="M"),aes(y=..count..*(-1)),width=1) + 
#  scale_y_continuous(breaks=seq(-1200,1200,200),labels=abs(seq(-1200,1200,200))) + 
  #  scale_x_discrete(breaks=seq(0,90,10))+
  ylab("")+
  xlab("Edad")+
  scale_fill_discrete(name="",labels=c("F","M"))+
  coord_flip()

ggsave("piramid_1869.png",dpi="retina",width=8,height=5)




# DESCRIPTIVAS MATCH ------------------------------------------------------

tibble(
  "Variables"=c(
    "strdist_FN",
      "strdist_LN",
      "hits",
      "hits^2",
      "exact",
      "exact_all",
      "gender_exact",
      "lsoundex",
      "fsoundex",
      "exact_mult",
      "f_name_subset",
      "middle_name_not_empty*strdist_mn_2",
      "second_lastname_not_empty*strdist_sln_2",
      "m_start",
      "relationship_offspring",
      "father_ln_not_empty*father_fn_not_empty",
      "mother_fn_not_empty*mother_ln_not_empty",
      "relationship_offspring*father_fn_not_empty*strdist_father_fn",
      "relationship_offspring*father_ln_not_empty*strdist_father_ln",
      "relationship_offspring*mother_fn_not_empty*strdist_mother_fn",
      "relationship_offspring*mother_ln_not_empty*strdist_mother_ln",
      "unique_gender",
      "unique_f_n_exact"
    
    
  ),
  
  "Fuente"=c(
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Propia",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    "Feigenbaum (2016)",
    rep("Propia",13)
  )
  ,
  
  
  "Resumen"=c(
    "Distancia de edición del primer nombre",
    "Distancia de edición del primer apellido",
    "Tamaño del bloque",
    "",
    "Hay al menos un individuo con el mismo nombre y apellido",
    "Número de individuos con el mismo nombre y apellido",
    "Coincidencia de género",
    "Distancia fonética del primer nombre",
    "Distancia fonética del primer apellido",
    "Existe al menos un individuo con el mismo nombre, apellido y género",
    "El primer nombre es un subconjunto",
    "Distancia de edición entre los segundos nombres si ambos los tienen",
    "Distancia de edición entre los segundos apellidos si ambos los tienen",
    "Coincidencia en primera letra del segundo nombre",
    "Ambos individuos son hijos de alguien",
    "Ambos individuos tienen información del primer nombre y apellido del padre",
    "Ambos individuos tienen información del primer nombre y apellido de la madre",
    "Distancia de edición entre el primer nombre de los padres si tienen información del padre y son hijos de alguien",
    "Distancia de edición entre el primer apellido de los padres si tienen información del padre y son hijos de alguien",
    "Distancia de edición entre el primer nombre de las madres si tienen información del padre y son hijos de alguien",
    "Distancia de edición entre el primer apellido de las madres si tienen información del padre y son hijos de alguien",
    "Es el único individuo del bloque para el cual coincide el género",
    "Es el único individuo del bloque para el cual coincide el primer nombre de forma exacta"
    
  )
  
  
)%>% 
  kable(.,escape = F,format="latex", booktabs=T) %>% 
  kable_styling()


