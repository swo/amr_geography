MODELS = ["whn", "dtypes"]

subworkflow db:
    workdir: "db"
    snakefile: "db/Snakefile"

rule all:
    input:
        # mechanistic simulations
        expand("fig/{model}_2pop.pdf", model=MODELS),
        "results/2pop_reduction.tsv",
        # grid simulation
        "fig/grid-sim-plot.pdf",
        "results/grid-sim-results.tsv",
        # empirical
        expand("fig/{x}.pdf", x=["cross_sectional"])

rule clean:
    shell: "rm -rf cache/* fig/* results/*"

rule empirical:
    output: "fig/cross_sectional.pdf"
    input:
        db("us/adjacency.tsv"), db("us/commuting.tsv"),
        db("europe/adjacency.tsv"), db("europe/flights.tsv"),
        expand("data/{x}/data.tsv", x=["ecdc", "marketscan", "nhsn"]),
        script="empirical_analysis.R"
    shell: "./{input.script}"

rule grid_sim:
    output: "fig/grid-sim-plot.pdf", "results/grid-sim-results.tsv"
    input: script="grid-sim.R"
    shell: "./{input.script}"

rule simulation_tables:
    output: "results/2pop_reduction.tsv"
    input: expand("cache/{model}_2pop.rds", model=MODELS), script="tables.R"
    shell: "./{input.script}"

rule twopop_fig:
    output: expand("fig/{model}_2pop.pdf", model=MODELS)
    input: expand("cache/{model}_2pop.rds", model=MODELS), "utils.R", script="2pop_figs.R"
    shell: "./{input.script}"

rule sim:
    output: "cache/{model}_2pop.rds"
    input: "utils.R", script="{model}_sims.R"
    shell: "./{input.script}"
