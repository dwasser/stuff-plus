## stuff-plus
#### Version 0.1 (stuff_plus.R)
- Limited to 4S fastballs, data from 2017-2023.
- Outcome: delta_run_exp, which is the change in run expectancy after the pitch. This is fine for starting the model, but really is too context-dependent (i.e. depends on base-out states).
- Features: velocity, spin rate, balls, strikes, batter stand, pitcher handedness, vertical movement, horizontal movement, 
- Learned how to set up, cross-validate, and operate xgboost model.
- Outcomes were generally sensible, but further refinement is necessary.
- Next version:
    - Still 4S only, same sample (2017-2023), train on 2023 only (b/c of rule changes, inspired by Eno Sarris podcast).
    - Implement separate models for swing, ball-in-play, groundball, flyball.
    - This is based on Fangraphs model: https://library.fangraphs.com/pitching/pitchingbot-pitch-modeling-primer/
    - Future versions: refine features and expand outcome predictions.
