# Artifact appendix examples (EuroSys 2022 / sysartifacts template)

Template: ../appendix_template.tex (from https://sysartifacts.github.io/eurosys2022/appendix/)
Instructions: https://sysartifacts.github.io/eurosys2022/instructions

## Template structure

```
A   Artifact Appendix
A.1 Abstract
A.2 Description & Requirements
    A.2.1 How to access
    A.2.2 Hardware dependencies
    A.2.3 Software dependencies
    A.2.4 Benchmarks
A.3 Set-up
A.4 Evaluation workflow
    A.4.1 Major Claims   (C1, C2 ... each -> experiment En -> paper table/figure)
    A.4.2 Experiments    (E1, E2 ... each: [time, disk] how-to, preparation, execution, results)
A.5 Notes on Reusability
A.6 General Notes
```

USENIX Security uses same template (secartifacts.github.io), adds A.2.1 Security/privacy/ethical concerns and A.3.1 Basic Test.

## Filled-in appendices (USENIX Sec 2023, 2-5 pages each)

All 140 at https://secartifacts.github.io/usenixsec2023/results . Sample downloaded here:

| file | paper | artifact |
|---|---|---|
| usenixsec23_sec23summerae-final1.pdf | Fuzztruction: Using Fault Injection-based Fuzzing to Leverage Implicit Domain Knowledge | https://github.com/fuzztruction/fuzztruction/tree/91ba684d2b8fa21ae19e403496b507f3729c4ff5 |
| usenixsec23_sec23summerae-final5.pdf | Improving Logging to Reduce Permission Over-Granting Mistakes | https://github.com/byshen/seclog_ae/releases/tag/v1.0 |
| usenixsec23_sec23summerae-final8.pdf | (M)WAIT for It: Bridging the Gap between Microarchitectural and Architectural Side Channels | https://github.com/cispa/mwait/tree/ae |
| usenixsec23_sec23summerae-final9.pdf | FuzzJIT: Oracle-Enhanced Fuzzing for JavaScript Engine JIT Compiler | https://github.com/SpaceNaN/fuzzjit/tree/a3d3f6da7f7f8577476892d6135eee6c50afc7ad |
| usenixsec23_sec23summerae-final13.pdf | Security and Privacy Failures in Popular 2FA Apps | https://github.com/blues-lab/totp-app-analysis-public/releases/tag/usenix-sec23-ae |
| usenixsec23_sec23summerae-final14.pdf | NVLeak: Off-Chip Side-Channel Attacks via Non-Volatile Memory Systems | https://github.com/TheNetAdmin/NVLeak/tree/588567e6ec30f2df9f260e60385031c94e94c75e |
| usenixsec23_sec23summerae-final16.pdf | Device Tracking via Linux's New TCP Source Port Selection Algorithm | https://github.com/0xkol/rfc6056-device-tracker/tree/09dd6ab68e10566eb6ca7760ef78d4689c7e2b85 |
| usenixsec23_sec23summerae-final17.pdf | Every Signature is Broken: On the Insecurity of Microsoft Office’s OOXML Signatures | https://github.com/RUB-NDS/OOXML_Signature_Security/releases/tag/Artifact_Evaluation |

## EuroSys 2022 artifacts (Distinguished Artifact winners, README-style, no separate appendix PDF public)

Results + zenodo DOIs: https://sysartifacts.github.io/eurosys2022/results

- eurosys22_safepm/ : SafePM (zenodo.6338745). artifact_evaluation/README.md has Set-up / Experiments (with hours per experiment) / Analysis (maps to figures) / Hardware / Software deps. figure_3.sh shows one-script-per-figure pattern.
- eurosys22_fgnn/ : GNNLab (zenodo.6347456). README has Hardware config / Install / Docker / QuickStart / Experiments.

## Tim's 10 repos vs template (checked 2026-09-14, README headings only, nothing executed)

None has an artifact appendix file. Closest to template: OptimizerTournament, ParetoMyth, NEO, Model-Instability (all have Requirements + step-by-step Reproducing with RQ->table/figure mapping). None state hardware, runtime per experiment, or explicit numbered claims. See ../papers/README.md.

## Generated appendix

- ../appendix/neo/neo_appendix.tex : EuroSys-template appendix for "How Low Can You Go? The Data-Light SE Challenge" (FSE'26, KKGanguly/NEO @ 5a0d487). Compiles to 2 pages via standalone.tex. Based on repo inspection + smoke test of `make report` and the two plot scripts only; optimizer re-runs not executed.
