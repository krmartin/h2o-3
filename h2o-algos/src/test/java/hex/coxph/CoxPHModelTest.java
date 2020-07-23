package hex.coxph;

import org.junit.BeforeClass;
import org.junit.Test;
import water.*;
import water.fvec.Chunk;
import water.fvec.Frame;
import water.fvec.TestFrameBuilder;
import water.fvec.Vec;
import water.util.RandomUtils;

import java.util.Random;

import static hex.coxph.CoxPHModel.concordance;
import static org.junit.Assert.*;

public class CoxPHModelTest extends TestUtil {
    
    @BeforeClass
    public static void setup() { stall_till_cloudsize(1); }

    @Test
    public void concordanceOfAPerfectEstimateIsOne() {
        checkConcordanceForEstimate(new MRTask() {
            @Override
            public void map(Chunk c) {
                for (int i = 0; i < c._len; ++i) {
                    c.set(i, c.atd(i) * -0.1);
                }
            }
        }, 1.0d, 0.0001d);
    }


    @Test
    public void concordanceOfATerribleEstimateIsZero() {
        checkConcordanceForEstimate(new MRTask() {
            @Override
            public void map(Chunk c) {
                for (int i = 0; i < c._len; ++i) {
                    c.set(i, c.atd(i));
                }
            }
        }, 0.0d, 0.0001d);
    }
    
    @Test
    public void concordanceOfARandomEstimateIsOneHalf() {
        checkConcordanceForEstimate(new MRTask() {
            @Override public void map(Chunk c){
                Random rng = new RandomUtils.PCGRNG(c.start(),1);
                for(int i = 0; i < c._len; ++i) {
                    c.set(i, rng.nextFloat());
                }
            }
        }, 0.5d, 0.01d); 
    }

    private void checkConcordanceForEstimate(MRTask estimateTask, double expected, double delta) {
        try {
            Scope.enter();
            final int len = 2000;

            Vec starts = Scope.track(Vec.makeCon(0.0, len));
            Vec times = Scope.track(Vec.makeCon(0.0, len).makeRand(0));
            Vec status = Scope.track(Vec.makeOne(len, Vec.T_CAT));

            new MRTask() {
                @Override
                public void map(Chunk c) {
                    for (int i = 0; i < c._len; ++i) {
                        c.set(i, 1L);
                    }
                }
            }.doAll(status);

            Frame aFrame = Scope.track(new Frame(new String[]{"starts", "times", "status"}, new Vec[]{starts, times, status}));
            Vec estimates = Scope.track(times.doCopy());
            Key<Vec> estimatesKey = estimates._key;
            DKV.put(estimatesKey, estimates);
            Scope.track(estimates);

            estimateTask.doAll(estimates);

            Frame scored = Scope.track(new Frame(estimates));

            final double c = concordance(aFrame, scored);

            assertEquals(expected, c, delta);
        } finally {
            Scope.exit();
        }
    }

    @Test
    public void concordanceExampleOneBadEstimate() throws Exception {
        try {
            Scope.enter();
            final Frame aFrame = Scope.track(new TestFrameBuilder()
                    .withName("testFrame")
                    .withColNames("Start", "Time", "Status")
                    .withVecTypes(Vec.T_NUM, Vec.T_NUM, Vec.T_NUM)
                    .withDataForCol(0, ard(0, 0, 0, 0, 0, 0, 0))
                    .withDataForCol(1, ard(0, 1, 2, 3, 4, 5, 6))
                    .withDataForCol(2, ard(1, 1, 1, 1, 1, 1, 1))
                    .withChunkLayout(7)
                    .build()); 
            
            final Frame scored = Scope.track(new TestFrameBuilder()
                    .withName("estimateFrame")
                    .withColNames("est")
                    .withVecTypes(Vec.T_NUM)
                    .withDataForCol(0, ard(6, 5, 4, 3, 2, 0, 1))
                    .build());

            final double c = concordance(aFrame, scored);
            final double pairCount = scored.numRows() * (scored.numRows() - 1) / 2d;
            assertEquals((pairCount - 1) / pairCount, c, 0.01);

        } finally {
            Scope.exit();
        }
    }
    
    @Test
    public void concordanceExampleMoreBadEstimates() throws Exception {
        try {
            Scope.enter();
            final Frame aFrame = Scope.track(new TestFrameBuilder()
                    .withName("testFrame")
                    .withColNames("Start", "Time", "Status")
                    .withVecTypes(Vec.T_NUM, Vec.T_NUM, Vec.T_NUM)
                    .withDataForCol(0, ard(0, 0, 0, 0, 0, 0, 0))
                    .withDataForCol(1, ard(0, 1, 2, 3, 4, 5, 6))
                    .withDataForCol(2, ard(1, 1, 1, 1, 1, 1, 1))
                    .withChunkLayout(7)
                    .build()); 
            
            final Frame scored = Scope.track(new TestFrameBuilder()
                    .withName("estimateFrame")
                    .withColNames("est")
                    .withVecTypes(Vec.T_NUM)
                    .withDataForCol(0, ard(6, 5, 4, 3, 0, 1, 2))
                    .build());

            final double c = concordance(aFrame, scored);
            final double pairCount = scored.numRows() * (scored.numRows() - 1) / 2d;
            assertEquals((pairCount - 3) / pairCount, c, 0.01);
        } finally {
            Scope.exit();
        }
    } 
    
    @Test
    public void concordanceExampleOneTie() throws Exception {
        try {
            Scope.enter();
            final Frame aFrame = Scope.track(new TestFrameBuilder()
                    .withName("testFrame")
                    .withColNames("Start", "Time", "Status")
                    .withVecTypes(Vec.T_NUM, Vec.T_NUM, Vec.T_NUM)
                    .withDataForCol(0, ard(0, 0, 0, 0, 0, 0, 0))
                    .withDataForCol(1, ard(0, 1, 2, 3, 4, 5, 6))
                    .withDataForCol(2, ard(1, 1, 1, 1, 1, 1, 1))
                    .withChunkLayout(7)
                    .build()); 
            
            final Frame scored = Scope.track(new TestFrameBuilder()
                    .withName("estimateFrame")
                    .withColNames("est")
                    .withVecTypes(Vec.T_NUM)
                    .withDataForCol(0, ard(6, 5, 4, 3, 2, 2, 0))
                    .build());

            final double c = concordance(aFrame, scored);
            final double pairCount = scored.numRows() * (scored.numRows() - 1) / 2d;
            assertEquals((pairCount - 0.5) / pairCount, c, 0.01);
        } finally {
            Scope.exit();
        }
    }

}
