import EctoEnum
defenum(Bex.UtxoType, :type, [:gold, :coin, :dust])

defenum(Bex.MissionType, :mission_type, [
  :meta_b,
  :meta_bcat_index,
  :meta_bcat_part,
  :mint,
  :recast
])

defenum(Bex.MissionStatus, :mission_status, [:onchain, :offchain])
