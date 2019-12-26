package edu.cmu.tetrad.algcomparison.algorithm.oracle.pag;

import edu.cmu.tetrad.algcomparison.algorithm.Algorithm;
import edu.cmu.tetrad.algcomparison.algorithm.MultiDataSetAlgorithm;
import edu.cmu.tetrad.algcomparison.score.BdeuScore;
import edu.cmu.tetrad.algcomparison.score.ScoreWrapper;
import edu.cmu.tetrad.algcomparison.score.SemBicScore;
import edu.cmu.tetrad.algcomparison.utils.HasKnowledge;
import edu.cmu.tetrad.algcomparison.utils.UsesScoreWrapper;
import edu.cmu.tetrad.annotation.AlgType;
import edu.cmu.tetrad.annotation.Bootstrapping;
import edu.cmu.tetrad.annotation.TimeSeries;
import edu.cmu.tetrad.data.DataModel;
import edu.cmu.tetrad.data.DataSet;
import edu.cmu.tetrad.data.DataType;
import edu.cmu.tetrad.data.IKnowledge;
import edu.cmu.tetrad.graph.EdgeListGraph;
import edu.cmu.tetrad.graph.Graph;
import edu.cmu.tetrad.search.BdeuScoreImages;
import edu.cmu.tetrad.search.IndTestScore;
import edu.cmu.tetrad.search.IndependenceTest;
import edu.cmu.tetrad.search.Score;
import edu.cmu.tetrad.search.SemBicScoreImages;
import edu.cmu.tetrad.search.TsDagToPag;
import edu.cmu.tetrad.search.TsGFci;
import edu.cmu.tetrad.util.Parameters;
import edu.cmu.tetrad.util.Params;
import edu.pitt.dbmi.algo.resampling.GeneralResamplingTest;
import edu.pitt.dbmi.algo.resampling.ResamplingEdgeEnsemble;
import java.util.ArrayList;
import java.util.List;

/**
 * tsIMaGES.
 *
 * @author jdramsey
 * @author Daniel Malinsky
 */
@edu.cmu.tetrad.annotation.Algorithm(
        name = "TsIMaGES",
        command = "ts-imgs",
        algoType = AlgType.forbid_latent_common_causes
)
@TimeSeries
@Bootstrapping
public class TsImages implements Algorithm, HasKnowledge, MultiDataSetAlgorithm, UsesScoreWrapper {

    static final long serialVersionUID = 23L;
    private ScoreWrapper score;
    private Algorithm initialGraph = null;
    private IKnowledge knowledge = null;

    public TsImages() {
    }

    public TsImages(ScoreWrapper score) {
        if (!(score instanceof SemBicScore || score instanceof BdeuScore)) {
            throw new IllegalArgumentException("Only SEM BIC score or BDeu score can be used with this, sorry.");
        }

        this.score = score;
    }

    @Override
    public Graph search(DataModel dataModel, Parameters parameters) {
        if (parameters.getInt(Params.NUMBER_RESAMPLING) < 1) {
            DataSet dataSet = (DataSet) dataModel;
            TsGFci search;
            if(knowledge != null) {
        		dataSet.setKnowledge(knowledge);
        	}
            Score score1 = score.getScore(dataSet, parameters);
            IndependenceTest test = new IndTestScore(score1);
            search = new TsGFci(test, score1);
            search.setKnowledge(dataSet.getKnowledge());
            search.setVerbose(parameters.getBoolean(Params.VERBOSE));
            
            return search.search();
        } else {
            TsImages algorithm = new TsImages(score);

            DataSet data = (DataSet) dataModel;
            GeneralResamplingTest search = new GeneralResamplingTest(data, algorithm, parameters.getInt(Params.NUMBER_RESAMPLING));
            search.setKnowledge(knowledge);

            search.setPercentResampleSize(parameters.getDouble(Params.PERCENT_RESAMPLE_SIZE));
            search.setResamplingWithReplacement(parameters.getBoolean(Params.RESAMPLING_WITH_REPLACEMENT));
            
            ResamplingEdgeEnsemble edgeEnsemble = ResamplingEdgeEnsemble.Highest;
            switch (parameters.getInt(Params.RESAMPLING_ENSEMBLE, 1)) {
                case 0:
                    edgeEnsemble = ResamplingEdgeEnsemble.Preserved;
                    break;
                case 1:
                    edgeEnsemble = ResamplingEdgeEnsemble.Highest;
                    break;
                case 2:
                    edgeEnsemble = ResamplingEdgeEnsemble.Majority;
            }
            search.setEdgeEnsemble(edgeEnsemble);
            search.setAddOriginalDataset(parameters.getBoolean(Params.ADD_ORIGINAL_DATASET));
            
            search.setParameters(parameters);
            search.setVerbose(parameters.getBoolean(Params.VERBOSE));
            return search.search();
        }
    }

    @Override
    public Graph getComparisonGraph(Graph graph) {
        return new TsDagToPag(new EdgeListGraph(graph)).convert();
    }

    public String getDescription() {
        return "tsFCI (Time Series Fast Causal Inference) using " + score.getDescription()
                + (initialGraph != null ? " with initial graph from "
                        + initialGraph.getDescription() : "");
    }

    @Override
    public DataType getDataType() {
        return score.getDataType();
    }

    @Override
    public List<String> getParameters() {
        List<String> parameters = new ArrayList<>();
        parameters.add(Params.NUM_RUNS);
        parameters.add(Params.RANDOM_SELECTION_SIZE);

        parameters.add(Params.VERBOSE);
        return parameters;
    }

    @Override
    public IKnowledge getKnowledge() {
        return this.knowledge;
    }

    @Override
    public void setKnowledge(IKnowledge knowledge) {
        this.knowledge = knowledge;
    }

    @Override
    public Graph search(List<DataModel> dataSets, Parameters parameters) {
        List<DataModel> dataModels = new ArrayList<>();

        for (DataModel dataSet : dataSets) {
            dataModels.add(dataSet);
        }

        TsGFci search;

        if (score instanceof SemBicScore) {
            SemBicScoreImages gesScore = new SemBicScoreImages(dataModels);
            gesScore.setPenaltyDiscount(parameters.getDouble(Params.PENALTY_DISCOUNT));
            IndependenceTest test = new IndTestScore(gesScore);
            search = new TsGFci(test, gesScore);
        } else if (score instanceof BdeuScore) {
            double samplePrior = parameters.getDouble(Params.SAMPLE_PRIOR, 1);
            double structurePrior = parameters.getDouble(Params.STRUCTURE_PRIOR, 1);
            BdeuScoreImages score = new BdeuScoreImages(dataModels);
            score.setSamplePrior(samplePrior);
            score.setStructurePrior(structurePrior);
            IndependenceTest test = new IndTestScore(score);
            search = new TsGFci(test, score);
        } else {
            throw new IllegalStateException("Sorry, data must either be all continuous or all discrete.");
        }

        IKnowledge knowledge = dataModels.get(0).getKnowledge();
        search.setKnowledge(knowledge);
        return search.search();
    }

    @Override
    public void setScoreWrapper(ScoreWrapper score) {
        this.score = score;
    }
    
    @Override
    public ScoreWrapper getScoreWrapper() {
        return score;
    }

}
