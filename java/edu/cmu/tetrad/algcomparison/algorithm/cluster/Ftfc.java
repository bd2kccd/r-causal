package edu.cmu.tetrad.algcomparison.algorithm.cluster;

import edu.cmu.tetrad.algcomparison.algorithm.Algorithm;
import edu.cmu.tetrad.algcomparison.utils.HasKnowledge;
import edu.cmu.tetrad.annotation.AlgType;
import edu.cmu.tetrad.data.*;
import edu.cmu.tetrad.graph.EdgeListGraph;
import edu.cmu.tetrad.graph.Graph;
import edu.cmu.tetrad.search.FindTwoFactorClusters;
import edu.cmu.tetrad.search.SearchGraphUtils;
import edu.cmu.tetrad.util.Parameters;
import edu.pitt.dbmi.algo.resampling.GeneralResamplingTest;
import edu.pitt.dbmi.algo.resampling.ResamplingEdgeEnsemble;

import java.util.ArrayList;
import java.util.List;

/**
 * FTFC.
 *
 * @author jdramsey
 */
@edu.cmu.tetrad.annotation.Algorithm(
        name = "FTFC",
        command = "ftfc",
        algoType = AlgType.search_for_structure_over_latents
)
public class Ftfc implements Algorithm, HasKnowledge, ClusterAlgorithm {

    static final long serialVersionUID = 23L;
    private IKnowledge knowledge = new Knowledge2();

    public Ftfc() {
    }

    @Override
    public Graph search(DataModel dataSet, Parameters parameters) {
        if (parameters.getInt("numberResampling") < 1) {
            ICovarianceMatrix cov = null;

            if (dataSet instanceof DataSet) {
                cov = DataUtils.getCovMatrix(dataSet);
            } else if (dataSet instanceof ICovarianceMatrix) {
                cov = (ICovarianceMatrix) dataSet;
            } else {
                throw new IllegalArgumentException("Expected a dataset or a covariance matrix.");
            }

            double alpha = parameters.getDouble("alpha");

            boolean gap = parameters.getBoolean("useGap", true);
            FindTwoFactorClusters.Algorithm algorithm;

            if (gap) {
                algorithm = FindTwoFactorClusters.Algorithm.GAP;
            } else {
                algorithm = FindTwoFactorClusters.Algorithm.SAG;
            }

            FindTwoFactorClusters search
                    = new FindTwoFactorClusters(cov, algorithm, alpha);
            search.setVerbose(parameters.getBoolean("verbose"));

            return search.search();
        } else {
            Ftfc algorithm = new Ftfc();

            //algorithm.setKnowledge(knowledge);
//          if (initialGraph != null) {
//      		algorithm.setInitialGraph(initialGraph);
//  		}
            DataSet data = (DataSet) dataSet;
            GeneralResamplingTest search = new GeneralResamplingTest(data, algorithm, parameters.getInt("numberResampling"));
            search.setKnowledge(knowledge);
            
            search.setPercentResampleSize(parameters.getDouble("percentResampleSize"));
            search.setResamplingWithReplacement(parameters.getBoolean("resamplingWithReplacement"));

            ResamplingEdgeEnsemble edgeEnsemble = ResamplingEdgeEnsemble.Highest;
            switch (parameters.getInt("resamplingEnsemble", 1)) {
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
            search.setAddOriginalDataset(parameters.getBoolean("addOriginalDataset"));
            
            search.setParameters(parameters);
            search.setVerbose(parameters.getBoolean("verbose"));
            return search.search();
        }
    }

    @Override
    public Graph getComparisonGraph(Graph graph) {
        return SearchGraphUtils.patternForDag(new EdgeListGraph(graph));
    }

    @Override
    public String getDescription() {
        return "FTFC (Find Two Factor Clusters)";
    }

    @Override
    public DataType getDataType() {
        return DataType.Continuous;
    }

    @Override
    public List<String> getParameters() {
        List<String> parameters = new ArrayList<>();
        parameters.add("alpha");
        parameters.add("useWishart");
        parameters.add("useGap");
        parameters.add("verbose");
        // Resampling
        parameters.add("numberResampling");
        parameters.add("percentResampleSize");
        parameters.add("resamplingWithReplacement");
        parameters.add("resamplingEnsemble");
        parameters.add("addOriginalDataset");
        return parameters;
    }

    @Override
    public IKnowledge getKnowledge() {
        return knowledge;
    }

    @Override
    public void setKnowledge(IKnowledge knowledge) {
        this.knowledge = knowledge;
    }

}
