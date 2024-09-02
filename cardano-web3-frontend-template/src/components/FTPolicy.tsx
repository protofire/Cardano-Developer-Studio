import { AppStateContext } from "@/pages/_app";
import {
  findUTxO,
  safeStringToBigInt,
  signAndSubmitTx,
} from "@/utilities/utilities";
import {
  fromText,
  getAddressDetails,
  MintingPolicy,
  PolicyId,
  Unit,
} from "lucid-cardano";
import { Data } from "lucid-cardano";
import { useContext, useState } from "react";

export default function Stablecoin() {
  const { appState } = useContext(AppStateContext);
  const { lucid, wAddr, contractType } = appState;
  const [tokenName, setTokenName] = useState("");
  const [amount, setAmount] = useState(10n);

  const redeemer = Data.Enum([
    Data.Literal("Mint"),
    Data.Literal("Burn"),
    Data.Literal("BadRedeemer"),
  ]);
  type Redeemer = Data.Static<typeof redeemer>;

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// MINT /////////////////////////////////////////////////////

  const mintTx = async () => {
    console.log("mintTx -> appState: ", appState);
    const tn = fromText(tokenName);

    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script:
        "585858560100003222325335533355333573466e1d20040011122200315333573466e1d20020011122200215333573466e1d2000001112220011533573892103505431001611220010040041120011635573a6ea800844880081",
    };
    if (!policyScript) {
      console.log("Policy Script not defined!");
      return;
    }
    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    console.log(tokenName, "   ", tn, "  ", unit);

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [unit]: amount },
        Data.to<Redeemer>("Mint", redeemer),
      )
      .attachMintingPolicy(policyScript)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// BURN /////////////////////////////////////////////////////

  const burnTx = async () => {
    console.log("burnTx -> appState: ", appState);
    const tn = fromText(tokenName);
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script:
        "585858560100003222325335533355333573466e1d20040011122200315333573466e1d20020011122200215333573466e1d2000001112220011533573892103505431001611220010040041120011635573a6ea800844880081",
    };
    if (!policyScript) {
      console.log("Policy Script not defined!");
      return;
    }

    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;

    if (!wAddr || !lucid || amount < 0n) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [unit]: -amount },
        Data.to<Redeemer>("Burn", redeemer),
      )
      .attachMintingPolicy(policyScript)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };

  ///////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////// ESPECIAL CASE ////////////////////////////////////////////

  const BadRedeemerTx = async () => {
    console.log("burnTx -> appState: ", appState);
    const tn = fromText(tokenName);
    const policyScript: MintingPolicy = {
      type: "PlutusV2",
      script:
        "585858560100003222325335533355333573466e1d20040011122200315333573466e1d20020011122200215333573466e1d2000001112220011533573892103505431001611220010040041120011635573a6ea800844880081",
    };
    if (!policyScript) {
      console.log("Policy Script not defined!");
      return;
    }

    const policyId: PolicyId = lucid!.utils.mintingPolicyToId(policyScript);
    const unit: Unit = policyId + tn;

    if (!wAddr || !lucid) return;
    const pkh: string = getAddressDetails(wAddr!).paymentCredential?.hash || "";

    const tx = await lucid!
      .newTx()
      .mintAssets(
        { [unit]: 1n },
        Data.to<Redeemer>("BadRedeemer", redeemer),
      )
      .attachMintingPolicy(policyScript)
      .addSignerKey(pkh)
      .complete();

    await signAndSubmitTx(tx);
  };
  ///////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////// UI /////////////////////////////////////////////////

  return (
    <div className="text-zinc-800 font-quicksand">
      {contractType == "policy" && (
        <div className="shadow-[0_4px_0px_0px_rgba(0,0,0,0.25)] w-[664px] bg-zinc-50 border border-zinc-600 rounded-xl p-9">
          <div className="w-full flex flex-row gap-4 mt-2">
            <p>Token name:</p>
            <input
              className="w-160 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="string"
              defaultValue={tokenName || ""}
              onChange={(e) => {
                const am = String(e.target.value);
                if (!am) return;
                setTokenName(am);
              }}
            />
            <p>Token amount (units):</p>
            <input
              className="w-16 py-1 px-2 ml-2 border border-zinc-700 rounded"
              type="number"
              value={Number(amount)}
              onChange={(e) => {
                const am = safeStringToBigInt(e.target.value);
                if (!am) return;
                setAmount(am);
              }}
            />
          </div>
          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={mintTx}
              disabled={!lucid || !wAddr || !amount || !tokenName}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Mint Tokens Tx
            </button>
            <button
              onClick={burnTx}
              disabled={!lucid || !wAddr || !amount || !tokenName}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Burn Tokens Tx
            </button>
          </div>
          <div className="w-full flex flex-row justify-center gap-4 mt-2">
            <button
              onClick={BadRedeemerTx}
              disabled={!lucid || !wAddr || !tokenName}
              className="w-full rounded-lg p-3 text-zinc-50 bg-zinc-800 shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200  disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600 font-quicksand font-bold active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)]"
            >
              {" "}
              Try Mint wsing a bad Redeemer Tx
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
