# Key numbers

*Generated 2026-09-03 from commit 45c33b9 by R/build_writing_pack.R.*

Every figure below is computed from the committed pipeline output. **Use these
values verbatim; do not recompute, round differently, or estimate.** If a number
the draft needs is not here, say so rather than inventing it.

## Specification

- Reference window: **2017-2021**
- Green-patent measure: **applications** (EPO, PATSTAT)
- Emissions/value added: **EXIOBASE 3.10.2** (Zenodo record 20051562)
- Countries: EU-27

## Headline diagnostics

| quantity | value |
|---|---:|
| cor(vulnerability, potential) | -0.55 |
| R2 vulnerability ~ log GDP p.c. | 0.27 |
| R2 potential ~ log GDP p.c. | 0.21 |
| eta2 vulnerability ~ development model | 0.40 |
| eta2 potential ~ development model | 0.41 |

## Within-block correlations (the two-part axis design)

| pair | r |
|---|---:|
| carbon intensity ~ energy intensity (twin) | 0.74 |
| GCI ~ GCP (twin) | 0.76 |
| intensity sub-index ~ fossil standalone | -0.01 |
| complexity sub-index ~ innovation standalone | 0.19 |

## Quadrant membership

| quadrant | n | countries |
|---|---:|---|
| Winners (low vuln / high pot) | 11 | Austria, Belgium, Denmark, Finland, France, Germany, Italy, Luxembourg, Slovenia, Spain, Sweden |
| At risk (high vuln / low pot) | 11 | Bulgaria, Croatia, Cyprus, Estonia, Greece, Hungary, Latvia, Lithuania, Malta, Romania, Slovakia |
| Exposed but capable | 3 | Czechia, Netherlands, Poland |
| Low-stakes / low capability | 2 | Ireland, Portugal |

Borderline (quadrant is convention-sensitive): **Ireland, Latvia, Slovakia, Slovenia**

## Quadrant x development model

| quadrant | Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|---:|
| At risk (high vuln / low pot) | 0 | 1 | 2 | 8 |
| Exposed but capable | 0 | 1 | 0 | 2 |
| Low-stakes / low capability | 0 | 1 | 1 | 0 |
| Winners (low vuln / high pot) | 6 | 1 | 3 | 1 |

## External validity (partial = net of log GDP p.c.)

| score | outcome | raw r | partial r |
|---|---|---:|---:|
| vulnerability | renew_share | -0.53 | -0.57 |
| vulnerability | gdp_growth | 0.30 | 0.20 |
| vulnerability | eps | -0.50 | -0.49 |
| potential | renew_share | 0.28 | 0.40 |
| potential | gdp_growth | -0.53 | -0.35 |
| potential | eps | 0.56 | 0.56 |

## Robustness: specification sensitivity

Spearman rank correlation vs the headline; quadrant changes out of 27.

| spec | cor vuln | cor pot | quadrant changes |
|---|---:|---:|---:|
| flat blocks (single PCA) | 0.71 | 0.90 | 8 |
| twin sub-index: mean not PCA | 1.00 | 1.00 | 0 |
| robust (median/MAD) scaling | 0.99 | 1.00 | 2 |
| part weights 2:1 (twin:standalone) | 0.95 | 0.98 | 2 |
| complexity: ECI replaces GCI | 1.00 | 0.92 | 0 |
| complexity: renewable-only GCI | 1.00 | 0.97 | 4 |
| fossil: production-based share | 0.70 | 1.00 | 4 |
| carbon: consumption-based (CBA) | 0.98 | 1.00 | 0 |
| patents: grants not applications | 1.00 | 1.00 | 0 |
| patents: log scale | 1.00 | 0.97 | 0 |
| window shift 2016-2020 | 1.00 | 0.99 | 2 |
| window shift 2018-2022 | 0.99 | 0.99 | 0 |

## Robustness: reference window (whole typology rebuilt on each)

| window | cor vuln | cor pot | quad changes | R2(v~GDP) | R2(p~GDP) | note |
|---|---:|---:|---:|---:|---:|---|
| 2017-2021 apps (HEADLINE) | 1.00 | 1.00 | 0 | 0.27 | 0.21 |  |
| 2014-2018 apps (former window) | 0.99 | 1.00 | 2 | 0.28 | 0.33 |  |
| 2014-2018 grants (former headline) | 0.99 | 0.99 | 2 | 0.28 | 0.35 |  |
| 2014-2017 apps (EORA-comparable) | 0.98 | 0.99 | 2 | 0.28 | 0.37 | matches EORA coverage |
| 2014-2017 grants (EORA-comparable) | 0.98 | 0.99 | 2 | 0.28 | 0.38 | matches EORA coverage |
| 2019-2021 apps (last 3y, clean) | 0.98 | 0.99 | 0 | 0.26 | 0.19 |  |
| 2020-2022 apps (last 3y, EXIOBASE max) | 0.97 | 0.98 | 0 | 0.26 | 0.18 | patent apps 2022 ~73% complete |
| 2017-2021 grants (truncated) | 1.00 | 1.00 | 0 | 0.27 | 0.21 | grants heavily grant-lag truncated |
| 2018-2022 PATSTAT v2 apps | 0.99 | 0.99 | 0 | 0.26 | 0.18 |  |

## Files in this pack

**Tables** (`evidence/`): 
`scores_by_country.csv`, `indicators_by_country.csv`, `quadrant_classification.csv`, `quadrant_profiles.csv`, `robustness_specs.csv`, `window_options.csv`, `validation_external.csv`, `validation_group_means.csv`, `burden_responsibility.csv`, `offshoring_origins.csv`, `development_model_gradient.csv`, `patent_options.csv`, `country_profiles.csv`, `energy_by_sector.csv`, `eora_crosscheck.csv`, `patent_offices.csv`, `capability_convergence.csv`, `validation_group_tests.csv`, `quadrant_stability.csv`, `group_composition.csv`, `quadrant_coherence.csv`, `validation_pairwise_tests.csv`

**Figures** (`figures/`): 
`typology_map.png`, `quadrant_profiles.png`, `validation_scores_by_group.png`, `validation_alluvial.png`, `burden_responsibility.png`, `offshoring_origins.png`, `window_options.png`, `patent_options.png`, `appendix_structure_map.png`, `country_profiles.png`, `eora_crosscheck.png`, `patent_offices.png`, `capability_convergence.png`, `quadrant_stability.png`, `group_composition.png`

