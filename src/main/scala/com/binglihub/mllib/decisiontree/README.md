# Decision Tree

### Impurity Functions

* Entropy

### Decision Tree Types

* ID3

## Example

```scala
import com.binglihub.mllib.decisiontree.DecisionTree
import com.binglihub.mllib.decisiontree.ImpurityFunc._
import com.binglihub.mllib.decisiontree.GainFunc._

val dt = new DecisionTree[String](entropy,id3)

dt.train(data)
dt.predict(record)

```