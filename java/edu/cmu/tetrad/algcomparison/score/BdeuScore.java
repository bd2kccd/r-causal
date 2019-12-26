package edu.cmu.tetrad.algcomparison.score;

import edu.cmu.tetrad.data.DataModel;
import edu.cmu.tetrad.data.DataType;
import edu.cmu.tetrad.data.DataUtils;
import edu.cmu.tetrad.graph.Node;
import edu.cmu.tetrad.search.Score;
import edu.cmu.tetrad.util.Parameters;
import edu.cmu.tetrad.util.Params;
import java.util.ArrayList;
import java.util.List;

/**
 * Wrapper for Fisher Z test.
 *
 * @author jdramsey
 */
@edu.cmu.tetrad.annotation.Score(
        name = "BDeu Score",
        command = "bdeu-score",
        dataType = DataType.Discrete
)
public class BdeuScore implements ScoreWrapper {

    static final long serialVersionUID = 23L;
    private DataModel dataSet;

    @Override
    public Score getScore(DataModel dataSet, Parameters parameters) {
        this.dataSet = dataSet;
        edu.cmu.tetrad.search.BDeuScore score
                = new edu.cmu.tetrad.search.BDeuScore(DataUtils.getDiscreteDataSet(dataSet));
        score.setSamplePrior(parameters.getDouble(Params.SAMPLE_PRIOR));
        score.setStructurePrior(parameters.getDouble(Params.STRUCTURE_PRIOR));
        return score;
    }

    @Override
    public String getDescription() {
        return "BDeu Score";
    }

    @Override
    public DataType getDataType() {
        return DataType.Discrete;
    }

    @Override
    public List<String> getParameters() {
        List<String> parameters = new ArrayList<>();
        parameters.add(Params.SAMPLE_PRIOR);
        parameters.add(Params.STRUCTURE_PRIOR);
        return parameters;
    }

    @Override
    public Node getVariable(String name) {
        return dataSet.getVariable(name);
    }
}
