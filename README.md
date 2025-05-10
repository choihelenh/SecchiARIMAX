# Tutorial


# Disclaimer of Limitations

It must be noted that this tool is site-specific, as the two reservoirs, Falling Creek
Reservoir (FCR) and Beaver Dam Reservoir (BVR) differ greatly in their chemical and
biological compositions. This model is specifically tuned for FCR, and thus its forecasts
should only be used for FCR and not be generalized to other reservoirs without retraining
the model to another reservoir's data.

The model's data should be continually updated, as relationships between environmental
variables and Secchi depth or with each other may change over time due to climate or land-use
shifts. Retraining the model at least annually and monitoring concept-drift diagnostics will
help reduce the effects of brittle modeling assumptions under a changing climate, especially
due to the model's fixed lags. Since we have automated the model, it will take in new data
each day to maintain accurate and relevant forecasts.

Water management operators and developers should also heavily consider the uncertainty of our
model, including its uncertainty bands and ensuring its numerical and visual outputs are not
treated as guaranteed forecasts, and taking these uncertainties into heavy consideration when
allocating resources or management strategies. These forecasts should also not be used as
the sole motivation for legislation or policy-making.
