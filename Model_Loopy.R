library(decisionSupport)
library (DiagrammeR)

# Impact Pathway ####
mermaid("graph TB
        NC(Nuttrees + chicken)
        NC-->CM(chicken mobile)
        NC-->PT(planting trees)
        NC-->IT(introduce truffle)
        NC-->PA(protective animals)
        NC-->IF(invest in fences)
        S(subsidies)-->I
        NC-->A(agrable land)
        A--> G(grassland)
        CM-->FC(feed costs)
        IT-->H(harvest)
        CM-->I(initial costs)
        IF-->I
        PA-->I
        PT-->MT(maintaining trees)
        MT-->O
        PT-->H
        PT-->I
        IT-->I
        G-->I
        H-->O(operating costs)
        FC-->O
        O-->C[costs]
        I-->C
        PT-->HQW(high quality wood)
        PT-->RN(regional Nuts)
        IT-->RT(regional truffle)
        CM-->FRE(free range eggs)
        CM-->AW(animal welfare)
        HQW-->B(benefits)
        RN-->B
        RT-->B
        FRE-->B
        HQW-->CS(CO2 sequestration)
        G-->BD(biodiversity)
        PT-->BD
        C-->NV(Net Present Value)
        B-->NV
        ")
