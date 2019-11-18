#!/bin/bash

# We assume running this from the script directory
project_dir=$HOME/Projects/SwissMADE
job_dir=$project_dir/.job
out_dir=$project_dir/.out

if [[ ! -e $job_dir ]]; then
    mkdir -p $job_dir
elif [[ ! -d $job_dir ]]; then
    echo "$job_dir already exists but is not a dir" 1>&2
fi

if [[ ! -e $out_dir ]]; then
    mkdir -p $out_dir
elif [[ ! -d $out_dir ]]; then
    echo "$out_dir already exists but is not a dir" 1>&2
fi

dbs=("chop" "cim10" "molis" "molis_smoker" "mouvement" "ofs" "predimed"
     "soarian_frcv" "soarian_med" "soarian" "soarian_sg_occ")

for db in ${dbs[@]}; do

    job_file="${job_dir}/${db}.job"

    echo "#!/bin/bash
#SBATCH --job-name=${db}.job
#SBATCH --output=${out_dir}/${db}.out
#SBATCH --error=${out_dir}/${db}.err
#SBATCH --time=2-00:00
#SBATCH --qos=normal
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jerome.pasquier@chuv.ch
#SBATCH --partition=cluster2
#SBATCH -n 1
#SBATCH --nice
Rscript ${project_dir}/id_var_20191008.R ${db}" > $job_file

    sbatch $job_file

done




