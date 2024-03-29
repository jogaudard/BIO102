Carbon flux cheat sheet
================
Joseph Gaudard
July 6, 2021

## Fluxes

By convention we look here at fluxes from the atmosphere perspective. Which means that a negative flux is a loss for the atmosphere and an uptake for the ecosystem. Positive fluxes are a gain for the atmosphere and an emission (loss) from the ecosystem. Note that the opposite would also be correct. That is why it is always necessary to indicate which convention you are using in your work.

### Net ecosystem exchange (NEE)

NEE is the amount of CO<sub>2</sub> that an ecosystem is uptaking (negative value) or emitting (positive value) when exposed to daylight. It is the net difference between ER and GEP. If ER is larger than GEP, the ecosystem is emitting CO<sub>2</sub> and therefore NEE has a positive value. If GEP is larger than ER, the ecosystem is uptaking CO<sub>2</sub> and therefore NEE has a negative value. NEE is being measured on the field by using the transparent chamber.

### Ecosystem respiration (ER)

ER is the total respiration from the ecosystem. It includes plant respiration and soil respiration (including microfauna). At our scale (40x25x25cm chamber), we don't include any other fauna than the soil microfauna. It is being measured on the field by excluding light (stopping any production) with a dark chamber (alternatively by covering the transparent chamber with a dark tarp). It is a positive flux.

### Gross ecosystem production (GEP)

GEP is the total production of the ecosystem. Meaning the photosynthesis done by all the plants of the ecosystem. It is a negative flux. GEP cannot be measured on the field as it is not possible to exclude ER. It is therefore calculated as the difference between NEE and ER.

## Environmental parameters

### Normalized Difference Vegetation Index (NDVI)

Put simply, NDVI is a measurement of "how green" the ecosystem is. It quantifies the difference between near infrared (NIR; strongly reflected by vegetation) and red light (asborbed by vegetation). It has a value between -1 and 1. The closer to 1, the denser the vegetation is.

<!-- $$NDVI= \frac{NIR-Red}{NIR+Red}$$ -->
<img src="https://render.githubusercontent.com/render/math?math=\color{violet}NDVI= \frac{NIR-Red}{NIR%2BRed}">

<!-- \begin{equation} -->
<!--   \text{NDVI} = \frac{\text{NIR}-\text{Red}}{\text{NIR}+\text{Red}} -->
<!-- \end{equation} -->
### Photosynthetically active radiation (PAR):

PAR defines the amount of light within the spectrum used for photosynthesis. The light used by plants to do photosynthesis is within the window 400 to 700nm wavelength. Here we use the photosynthetic photon flux (PPF) which values all photon equally. In reality, plants absorb different wavelength differently and not all the wavelength carry the same amount of energy. To take this into account one would need to use yield photon flux (YPF), which is a more complicated story. As PAR is an energy measurement, its usual unit is *W*/*m*<sup>2</sup>. But in plant physiology it makes more sense to use *m**o**l*/*m*<sup>2</sup>/*s* as it reflects better the availability of light for plants.

## Calculations

<img src="https://render.githubusercontent.com/render/math?math=\color{violet}flux=slope\times \frac{P\times V}{R\times T\times A}">

Where:

-   flux: the flux of CO<sub>2</sub> at the surface of the plot (*μ*mol m<sup>-2</sup> s<sup>-1</sup>)
-   slope: slope of linear regression fitting the CO<sub>2</sub> concentration versus time (*μ*mol mol<sup>-1</sup>)
-   P: pressure, assumed 1 atm
-   V: volume of the chamber and tubing (L)
-   R: gas constant (0.082057 L atm K<sup>-1</sup> mol<sup>-1</sup>)
-   T: chamber air temperature (K)
-   A: area of chamber frame base (m<sup>2</sup>)

<!-- \begin{equation} -->
<!-- \label{flux} -->
<!--  \text{flux}=\text{slope}\times \frac{P\times V}{R\times T\times A} -->
<!-- \end{equation} -->
<!-- Where: -->
<!-- \hspace*{3em} -->
<!-- \begin{itemize} -->
<!-- \item flux: gas flux (\(mmol/m^2/h\)) -->
<!--  \item slope: slope of the trend line  of the concentration of CO\textsubscript{2} versus time (\(ppm \times h^{-1}\)) -->
<!--  \item \(P\): pressure, assumed 1 \(atm\) -->
<!--  \item \(V\): volume of the chamber and tubing (\(L\)) -->
<!--  \item \(R\): gas constant (0.082057 \(L\times atm\times K^{-1}\times mol^{-1}\)) -->
<!--  \item \(T\): chamber air temperature (\(K\)) -->
<!--  \item \(A\): area of plot (\(m^2\)) -->
<!-- \end{itemize} -->
