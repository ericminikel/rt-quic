# a simple function to plan out volumes of inputs into master mix for RT-QuIC reactions

master_mix_planner = function (
    n_wells, # number of wells used
    input_rprp, # filtered rprp concentration in mg/mL
    excess = .2, # excess master mix to make just to be sure you have enough, e.g. .2 means 20% extra
    final_rprp = .1, # desired final rprp conc in mg/mL
    input_nacl = 2000, # input NaCL in mM
    final_nacl = 300, # desired final salt concentration including PBS, in mM
    input_edta = 100, # input EDTA in mM
    final_edta = 1, # mM
    input_tht = 1000, # uM. this default assumes you dilute the stock 1:10 before making master mix
    final_tht = 10, # uM
    input_pbs = 5, # X
    final_pbs = 1, # X
    well_volume = 100, # uM
    bh_volume = 2 # uM
    ) {
    pbs_nacl = 170 # concentration of NaCl in PBS in mM
    fudge_factor = 1 + excess
    vol_total = (well_volume - bh_volume) * n_wells * fudge_factor
    vol_pbs  = vol_total * final_pbs / input_pbs
    vol_tht  = vol_total * final_tht / input_tht
    vol_edta = vol_total * final_edta / input_edta
    vol_rprp = vol_total * final_rprp / input_rprp
    vol_nacl = vol_total * (final_nacl - pbs_nacl * final_pbs) / input_nacl
    vol_water = vol_total - vol_pbs - vol_tht - vol_edta - vol_rprp - vol_nacl
    cat(paste("PBS ",input_pbs,"X: ",round(vol_pbs), "uL\n",sep=""))
    cat(paste("ThT ",input_tht,"uM: ",round(vol_tht), "uL\n",sep=""))
    cat(paste("EDTA ",input_edta,"mM: ",round(vol_edta), "uL\n",sep=""))
    cat(paste("NaCl ",input_nacl,"mM: ",round(vol_nacl), "uL\n",sep=""))
    cat(paste("rPrP ",input_rprp,"mg/mL: ",round(vol_rprp), "uL\n",sep=""))
    cat(paste("water: ",round(vol_water), "uL\n",sep=""))
    cat(paste("TOTAL VOLUME: ",round(vol_total), "uL\n",sep=""))
}

# example usage:
master_mix_planner(input_rprp=.6,n_wells=96)