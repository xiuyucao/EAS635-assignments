select
  t.cn as TREE_CN,
  t.ht,
  t.dia,
  t.cr,
  t.cclcd,
  t.drybio_stem,
  t.drybio_stem_bark,
  t.drybio_branch,
  t.drybio_foliage,
  rs.common_name,
  rs.genus,
  rs.species,
  p.cn as PLT_CN,
  p.lat,
  p.lon,
  p.countycd,
  p.measyear,
  c.dstrbcd1,
  c.dstrbyr1
from plot p
  join cond c on p.cn = c.plt_cn
  join tree t on p.cn = t.plt_cn and c.condid = t.condid
  join ref_species rs on t.spcd = rs.spcd
where
  p.statecd = 26
  and statuscd = 1
  and p.measyear > 1999
  