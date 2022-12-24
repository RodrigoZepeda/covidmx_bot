eval `ssh-agent -s` && ssh-add ~/.ssh/github && ssh-add -l
cd /home/rod/covidmx_plots
date=$(date '+%Y-%m-%d')
/usr/bin/R < /home/rod/covidmx_plots/generate_state_plots.R --no-save
/usr/bin/git -C /home/rod/covidmx_plots add .
/usr/bin/git -C /home/rod/covidmx_plots commit -m "Actualización automática ${date}"
/usr/bin/git -C /home/rod/covidmx_plots push origin main
