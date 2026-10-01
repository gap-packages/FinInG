This file describes changes in the FinInG package.

## 1.5.6.1 (unreleased)

- Add `PointsOfQuadraticVariety`, which `Points` now uses for quadratic
  varieties, computing the points via the associated polar space
- Fix polynomial variable names of polar spaces (`x1` instead of `x_1`)
  depending on which polar spaces were created before over the same field
- Make `SegreMapsFamily` and `SegreMapsType` global variables
- Require GAP >= 4.12, as only that ships with a new enough cvec (#44)
- Update the introduction and other parts of the manual

## 1.5.6 (2023-07-27)

- Declare `UnderlyingObject` as an attribute

## 1.5.5 (2023-02-09)

- Support `UnderlyingObject` being declared as an attribute by other packages

## 1.5.4 (2022-12-13)

- Janitorial changes

## 1.5.3 (2022-12-10)

- Remove the outdated version number from the banner

## 1.5.2 (2022-12-10)

- Print the progress output of `FiningOrbits` only if the info level of
  `InfoFinInG` is positive

## 1.5.1 (2022-09-21)

- Require cvec >= 2.7.6, which fixes `SingerCycleCollineation` for certain
  arguments
- Fix the documentation of `3D4fining`, which mentioned a nonexistent argument
  d, and many typos in the manual

## 1.5 (2022-07-10)

- Fix an unexpected error in `ElationOfProjectiveSpace`
- Fix `ProjectiveElationGroup` with arguments sub and centre failing for a
  centre given by a compressed vector
- Fix `BLTSetByqClan`, which used the bilinear form of the ambient Q(4,q)
  instead of its quadratic form
- Require only GRAPE >= 4.8.2, restoring compatibility with GAP 4.10
- Fix LaTeX errors in the manual
- Point the package URLs at GitHub

## 1.4.2 (2020-07-03)

- Add subgeometries of projective spaces
- Add methods for `EvaluateForm` and `\^` for forms and subspaces of
  projective spaces
- Speed up `ShadowOfFlag` and the enumeration of shadows of flags for polar
  spaces
- Fix `ShadowOfFlag` for projective spaces computing the size of the shadow
  wrongly in some cases
- Fix `\in` for shadow elements, which ignored the parent flag, so that e.g. a
  point outside a plane was found in `Points` of that plane
- Fix some wrong `PrintObj` delegations for varieties
- Require GAP >= 4.10 and Forms >= 1.2.5

## 1.4.1 (2018-03-31)

## 1.4 (2017-11-23)

## 1.3.7 (2017-10-11)

## 1.3.5 (2016-02-16)

## 1.3.4 (2016-02-16)

## 1.3.3 (2016-02-16)

## 1.3.2 (2016-01-19)

## 1.3 (2015-11-30)

## 1.2 (2015-11-27)

## 1.1 (2015-11-09)

## 1.0 (2014-09-19)
